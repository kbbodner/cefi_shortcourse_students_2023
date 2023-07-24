library(parallel)
library(foreach)

#' Global options setup for using tmbstan with McMasterPandemic
#'
#' This is a hack: compiling the C++ code for tmbstan instead of using package-compiled objects, which doesn't work
#'
#' @return NULL
#' @export
setup_stan = function(){
  cpp_dir = system.file('tmb', options()$MP_flex_spec_version, package = "McMasterPandemic")
  set_spec_version(options()$MP_flex_spec_version, cpp_dir, use_version_directories = FALSE)
}

#' Calibrate a flexmodel using Stan
#'
#' @param model a [McMasterPandemic::flexmodel] object, the precursor model to the next argument
#' @param model_to_calibrate a [McMasterPandemic::flexmodel_to_calibrate] object
#' @param ... arguments to pass to [tmbstan::tmbstan] for the MCMC method, like `chain`, `iter`, `init`, `seed`
#'
#' @return a list with three elements
#'  - `model`: the input object
#'  - `model_to_calibrate`: the input object
#'  - `fit`: the fit as a [rstan::stanfit] object
#' @export
calibrate_stan = function(
  model,
  model_to_calibrate,
  ...
){
  # extract tmb object
  model_tmb = McMasterPandemic::tmb_fun(model_to_calibrate)

  fit = tmbstan::tmbstan(
    model_tmb,
    ...
  )

  # return list with output
  list(
    model = model,
    model_to_calibrate = model_to_calibrate,
    fit = fit
  )
}

#' Tidy fit component output from [calibrate_stan()]
#'
#' @param model_fit list. the output of [calibrate_stan()].
#'
#' @return a list with two elements
#'  - `model`: the input object
#'  - `model_to_calibrate`: the input object
#'  - `fit`: the fit as a [rstan::stanfit] object
tidy_fit_stan = function(
    model_fit
){
  McMasterPandemic::unpack(model_fit)

  # rename params
  names(fit) = c(
    names(tmb_params_trans(model_to_calibrate)),
    "log_posterior"
  )

  return(
    list(
      model = model,
      model_to_calibrate = model_to_calibrate,
      fit = fit
    )
  )
}

#' Forecast ensemble using Stan fit
#'
#' @param model_fit list. the output of [calibrate_stan()].
#' @param days_to_forecast numeric. (optional) the number of days to forecast in the future. if NULL, just simulate over the calibration period.
#' @param params_timevar data frame. (optional) time-varying parameters for forecast period. must have columns `Date`, `Symbol`, `Value`, and `Type`. see [here](https://canmod.github.io/macpan-book/time-varying-parameters.html#model-of-piece-wise-time-variation) for more details. if `NULL`, will simulate without time-varying parameter changes in the forecast period.
#' @param parallel logical. should the ensemble simulation be run in parallel?
#' @param n_cores integer. number of cores to use for parallel simulation.
#'
#' @return a list with two elements:
#'    - `model`: the input object from model_fit$model
#'    - `ensemble`: a data.frame with individual realizations in the ensemble
#' @export
ensemble_stan = function(
  model_fit,
  days_to_forecast = NULL,
  params_timevar = NULL,
  parallel = TRUE,
  n_cores = 2
){
  
  # unpack model and fit into local environment
  McMasterPandemic::unpack(model_fit)

  # collect and label Stan samples
  ext = rstan::extract(fit)
  if(!is.null(ext$params)){
    samples = as.matrix(ext$params)
  } else {
    samples = NULL
  }
  if(!is.null(ext$tv_mult)){
    samples_tv = as.matrix(ext$tv_mult)
  } else {
    samples_tv = NULL
  }
  
  fit_names = names(McMasterPandemic::tmb_params_trans(model_to_calibrate))
  
  if(!is.null(samples)){
    colnames(samples) = fit_names[1:ncol(samples)]
  }
  
  if(!is.null(samples_tv)){
    ncol_samples = ifelse(is.null(samples), 0, ncol(samples))
    colnames(samples_tv) = fit_names[(ncol_samples+1):length(fit_names)]
  }

  # simulate from Stan samples in parallel
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  # register parallelization or sequential computation
  if(parallel){
    doParallel::registerDoParallel(cores = n_cores)
  } else{
    foreach::registerDoSEQ()
  }

  # update model with forecast info

  if(!is.null(days_to_forecast)){
    model = (model
    # extend simulation end date
      %>% McMasterPandemic::extend_end_date(
        days_to_extend = days_to_forecast
      )
    )
  }

  if(!is.null(params_timevar)){
    model = (model
     # attach time-varying parameters for forecast scenario
      %>% McMasterPandemic::add_piece_wise(
        params_timevar = params_timevar
      )
    )
  }

  # loop over samples, update model with sampled params, and simulate
  sims = foreach(i=c(1:max(nrow(samples), nrow(samples_tv))), # max in case one of these arrays is NULL
                 .packages=c('McMasterPandemic', 'magrittr')) %dopar% {
    params = McMasterPandemic::invlink_trans(samples[i,])
    params_tv = McMasterPandemic::invlink_trans(samples_tv[i,])

    # update model with samples

    # update fitted params, if they exist
    if(length(params)!=0){
      model = (model
       # update model with sample params
       %>% McMasterPandemic::update_params(
         # back-transform any transformed values
         params
       ))
    }
    
    if(length(params_tv)!=0){
      sched = model$timevar$piece_wise$schedule
      
      # parse scheduled value updates
      sched_updates = (data.frame(Value = params_tv)
       %>% mutate(Symbol = rownames(.))
       %>% separate_wider_regex(Symbol, 
            patterns = c(Symbol = "^.*",
                         "_t(?=\\d+)",
                         breaks = "\\d+$")
          )
       %>% mutate(breaks = as.numeric(breaks))
      )
      
      # attach new values and update where they exist
      sched_new = (left_join(
        sched, sched_updates, 
        by = c("Symbol", "breaks"), suffix = c("", ".new"))
        %>% mutate(Value = case_when(
          !is.na(Value.new) ~ Value.new,
          T ~ Value
        ))
        %>% select(-Value.new)
      )
      
      # update model with new time-varying parameters
      model$timevar$piece_wise$schedule = sched_new
    }
    
    # simulate
    sim = McMasterPandemic::simulation_history(model, obs_error = TRUE)
  }

  # return list with model and sims (raw iterations)
  list(
    model = model_fit$model,
    ensemble = sims
  )
}

order_vars = function(
  model
){
  state_var = McMasterPandemic::topological_sort(model)
  other_var = names(model$sim_report_exprs)
  var_levels = c(state_var, other_var)
}

#' Summarise ensemble
#'
#' @param ens list. output from [ensemble_stan()].
#' @param qvec named numeric vector giving quantiles to compute for summary.
#'
#' @return a data.frame with the ensemble summary
#' @export
summarise_ensemble_stan = function(
    ens,
    var_order,
    qvec = c(value = 0.5, lwr = 0.025, upr = 0.975)
){
  # generate initial summary
  summ = (ens$ensemble
    %>% summarise_trajectories(qvec = qvec)
    # remove rates
    %>% dplyr::filter(!stringr::str_detect(var, "_to+_"))
  )

  # make variable levels based on state variables in compartmental order
  # and then everything else
  var_levels = order_vars(ens$model)

  # return summary
  (summ
    %>% dplyr::mutate(var = factor(var, levels = var_levels))
    %>% dplyr::rename(date = Date)
    %>% dplyr::arrange(date, var)
   )
}

#' Helper function for [summarise_ensemble_stan]
#'
#' @param trajectories individual simulations for ensemble
#' @param qvec named vector of quantiles for summary (value, lwr, upr)
#'
#' @return
summarise_trajectories = function(trajectories, qvec){
  summarised_traj = (trajectories
    %>% dplyr::bind_rows(.id = 'simulation')
    %>% tidyr::pivot_longer(c(-simulation, -Date), names_to = 'var')
    %>% dplyr::group_by(Date, var)
    %>% do(setNames(data.frame(t(quantile(.$value, probs = qvec, na.rm = TRUE))), names(qvec)))
  )
  attr(summarised_traj, "qvec") = qvec
  summarised_traj
}

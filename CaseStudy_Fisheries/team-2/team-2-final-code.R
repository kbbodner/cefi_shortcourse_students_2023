# main script to run all files that do the analysis and make figures ===========
library(here)

source(here("./CaseStudy_Fisheries/team-2/R/01_preparation.R"))
source(here("./CaseStudy_Fisheries/team-2/R/02_covariate_selection.R"))
source(here("./CaseStudy_Fisheries/team-2/R/03a_baseline_ricker.R"))
source(here("./CaseStudy_Fisheries/team-2/R/03b_ricker_env_covariate.R"))
source(here("./CaseStudy_Fisheries/team-2/R/03c_naive_model.R"))
source(here("./CaseStudy_Fisheries/team-2/R/03d_multiple_model.R"))
source(here("./CaseStudy_Fisheries/team-2/R/03e_power_model.R"))
source(here("./CaseStudy_Fisheries/team-2/R/03f_LM_naive_model.R"))
source(here("./CaseStudy_Fisheries/team-2/R/04_performance.R"))
source(here("./CaseStudy_Fisheries/team-2/R/05_power_uncertainty.R"))
source(here("./CaseStudy_Fisheries/team-2/R/06_graphs.R"))

## Case Study: Forecasting fish harvests 
Purpose: compute and compare three different forecasting models (Basic-Ricker, Power, Power with an environmental covariate) to predict the number of Salmon recruiters in the Early Stewart population in 2023. 

Team: Jake Lawlor, Yiran Wang, Emmerson Wilson, and Garland Xie

## Layman summary

Canadians rely on healthy Salmon fish populations to ensure food security, income, healthy ecosystems, and cultural and spiritual use. A common thread across these benefits is appropriate levels of fish harvest. However, decisions on fish harvests are challenging because of the delicate balance between current needs and healthy populations in the future. To address this issue, we must predict the number of Salmons that return back to the rivers they were born in to reproduce across different environments. This allows us to guess how large the future generations of salmon will be, and the number of salmon humans can harvest. Using the Early Stewart River as a case study, we predicted historically small populations in 2023. There was a large landslide that blocked the return of salmon to this river, which is the likely cause for the small population size. Environmental changes also matter. When leaving this river when they are young, warmer sea surface temperatures lead to low food quality, and this led to even lower fish populations. Here, we recommend limiting fishing to the extent possible. This will hopefully allow the population to recover so that, in the future, we can maintain a healthy population of salmon providing many economic and cultural benefits for Canadians.

## Methods

All materials for fisheries case study on forecasting Fraser Sockeye Salmon returns.

Data includes spawner-recruit data for all Fraser Sockeye stocks with available data, that rear in freshwater for for two winters (Harrison stock removed).

Potential environmental covariates are also included, which are generally assumed to affect early-marine survival, so are synced with each brood-years outmigration year and approximate timing.

All provided code can be found in "Code/Functions.R". 

### Methods: Basic Ricker model 

The Ricker model is a discrete stock-recruitment model that incorporates population growth ($\alpha$) and carrying capacity ($\beta$) for Pacific Salmon. Of all the available stocks, Early Stewart population was randomly chosen since it is one of many that is sensitive to environmental changes (see below models).

It assumes the chosen population experiences density dependence.

Consider the following model:

$$
R_i = \alpha S_i e^{-\beta S_i} 
$$

We can linearize to obtain simpler parameters:

$$
ln(R_i) = ln(\alpha)+ ln(S_i)-S_i/S_{max}
$$

We place priors on the following:

$$
\widehat{lnR_i} \sim normal(ln(R_i), \tau)
$$

$$
ln(\alpha) \sim normal(\mu_{\alpha}, \tau{\alpha}) 
$$

$$
ln(S\_{max}) \sim normal(\mu_{Smax}, \tau_{Smax})
$$

$$
\tau \sim gamma(\theta, \theta)
$$


## Deliverables



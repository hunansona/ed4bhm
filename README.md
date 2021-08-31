# ed4bhm
Empirical Determinacy for Bayesian Hierarchical Models

The popular Bayesian meta-analysis expressed by Bayesian NNHM synthesizes knowledge from several studies and is highly relevant in practice. Moreover, NNHM is the simplest Bayesian hierarchical model (BHM), which illustrates typical problems in more complex BHMs. Until now, it is unclear to what extent data determine the marginal posterior distributions of parameters in NNHM. To address this issue we computed the second derivative of Bhattacharyya coefficient with respect to the weighted likelihood, defined the total empirical determinacy (TED), and its proportion for location (pEDL) and spread (pEDS). We implemented this method in R package ed4bhm and considered two case studies and one simulation study. We focused on NNHM and quantified TED, pEDL and pEDS under different modeling conditions such as model parametrization, the primary outcome, and the prior. This clarified to what extent the location and spread of the marginal posterior distribution of parameters are determined by the data. Although these investigations focused on Bayesian NNHM, the method proposed is applicable more generally to complex BHMs. 


*********************************************************************************
How to install an R package that is sitting on github to R (for example, from Rstudio)?

> install.packages("devtools")
> library("devtools")
> install_github("hunansona/ed4bhm") 

You might need 
> Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
> install_github("hunansona/ed4bhm") 


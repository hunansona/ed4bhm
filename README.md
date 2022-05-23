# ed4bhm
Empirical Determinacy for Bayesian Hierarchical Models

Until now, it is unclear to what extent data determine the marginal posterior distributions of parameters of Bayesian hierarchical models (BHM). To address this issue we compute the second derivative of the Bhattacharyya coefficient with respect to the weighted likelihood, defined the total empirical determinacy (TED), and its proportion for location (pEDL) and spread (pEDS). This method is implemented in the R package ed4bhm for BHMs fit by INLA, JAGS, and Stan. The empirical determinacy estimates (TED, pEDL, pEDS) clarify to what extent the location and spread of the marginal posterior distribution of parameters are determined by the data. Moreover, we provide bootstrap-based Monte Carlo standard errors of empirical determinacy estimates based on MCMC samples provided by JAGS and Stan.

*********************************************************************************
How to install an R package that is sitting on github to R (for example, from Rstudio)?

> install.packages("devtools")
> library("devtools")
> install.packages("withr")
> library(withr)
> install_github("hunansona/ed4bhm") 

You might need to set
> Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
> install_github("hunansona/ed4bhm") 



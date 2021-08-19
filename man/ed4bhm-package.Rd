\name{ed4bhm-package}
\alias{ed4bhm-package}
\alias{ed4bhm}
\docType{package}
\title{
\packageTitle{Empirical Determinacy for Bayesian hierarchical models}
}
\description{
\packageDescription{ed4bhm}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{The popular Bayesian meta-analysis expressed by Bayesian NNHM synthesizes knowledge from several studies and is highly relevant in practice. 
Moreover, NNHM is the simplest Bayesian hierarchical model (BHM), which illustrates typical problems in more complex BHMs.
Until now, it is unclear to what extent data determine the marginal posterior distributions of parameters in NNHM. 
To address this issue we computed the second derivative of Bhattacharyya coefficient with respect to the weighted likelihood, defined the total empirical determinacy (TED), and its proportion for location (pEDL) and spread (pEDS).
We implemented this method in R package ed4bhm and considered two case studies and one simulation study.
We focused on NNHM and quantified TED, pEDL and pEDS under different modeling conditions such as model parametrization, the primary outcome, and the prior.
This clarified to what extent the location and spread of the marginal posterior distribution of parameters are determined by the data.
Although these investigations focused on Bayesian NNHM, the method proposed is applicable more generally to complex BHMs.}
\packageIndices{ed4bhm}
Apply ed.inla and ed.jags functions to objects provided by INLA and JAGS, respectively.
}
\author{
\packageAuthor{Sona Hunanyan [aut, cre], Malgorzata Roos [aut]}

Maintainer: \packageMaintainer{Sona Hunanyan <sona.hunanyan@uzh.ch>}
}
\references{
Hunanyan, S., Plummer, M., Rue, H., Roos, M., (2021). Quantification of empirical determinacy: the impact of likelihood weighting on posterior location and spread in Bayesian meta-analysis estimated with JAGS and INLA. Bayesian Analysis (in preparation). 
}
\keyword{ Empirical determinacy }
\examples{
#The effect of a coaching program on SAT-V (Scholastic Aptitude Test-Verbal) #scores in eight high schools, data from a randomized study were pre-analyzed and #supplied for a Bayesian meta-analysis. The eight schools data has been used to #study the performance of the Bayesian NNHM and to demonstrate typical issues, #which arise for BHMs (Gelman and Hill, 2007, Gelman et al., 2014, Vehtari et al#., 2021]).

data(eight_schools)

#prior settings
mean_mu<-0
prec_mu<-1/(4^2) 
prec_tau<-1/(5^2)

library(INLA)
HN.prior = "expression:
  tau0 =  1/(5^2);
  sigma = exp(-theta/2);
  log_dens = log(2) - 0.5 * log(2 * pi) + 0.5 * log(tau0);
  log_dens = log_dens - 0.5 * tau0 * sigma^2;
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);
"

formula.8schools.HN <- y ~ 1+f(schooln, model="iid", 
                                      hyper = list(prec = list(prior = HN.prior)))


# INLA uses the centered parametrization of the eight schools model by default
fit.inla.8schools <- inla(formula.8schools.HN,
                                data = data_8schools,
                                family = "gaussian",
                                scale = data_8schools$prec,
                                control.family = list(hyper=list(prec=list(initial = log(1), fixed=TRUE))),
                                control.fixed = list(mean.intercept=mean_mu, prec.intercept = prec_mu),
                                control.compute=list(hyperpar=TRUE),
                                num.threads=1)

del <- 0.01 #weighting factor w = 1 + del, w = 1 - del
#estimates the matrix of descriptive statistics given INLA base fit object
descriptives_inla_8schools <- extract_descriptives_mbp_inla(inla.obj = fit.inla.8schools, delta = del)
#estimates the empirical determinacy measure given the base INLA fit object 
ed_inla_8schools <- ed.inla(inla.object.base = fit.inla.8schools, delta = del, distance = "H2ALL")
}

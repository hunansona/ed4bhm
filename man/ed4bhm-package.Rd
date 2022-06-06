\name{ed4bhm-package}
\alias{ed4bhm-package}
\alias{ed4bhm}
\docType{package}
\title{
\packageTitle{ed4bhm}
}
\description{
\packageDescription{ed4bhm}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{Until now, it is unclear to what extent data determine the marginal posterior distributions of parameters of Bayesian hierarchical models (BHM). To address this issue we compute the second derivative of the Bhattacharyya coefficient with respect to the weighted likelihood, defined the total empirical determinacy (TED), and its proportion for location (pEDL) and spread (pEDS). This method is implemented in the R package ed4bhm for BHMs fit by INLA, JAGS, and Stan. The empirical determinacy estimates (TED, pEDL, pEDS) clarify to what extent the location and spread of the marginal posterior distribution of parameters are determined by the data. Moreover, we provide bootstrap-based Monte Carlo standard errors of empirical determinacy estimates based on MCMC samples provided by JAGS and Stan.}
\packageIndices{ed4bhm}
Apply ed.inla, ed.jags, and ed.stan functions to objects provided by INLA, JAGS, and Stan, respectively.
}
\author{
\packageAuthor{ed4bhm}

Maintainer: \packageMaintainer{ed4bhm}
}
\references{
Hunanyan, S., Plummer, M., Rue, H., Roos, M., (2021). Quantification of empirical determinacy: the impact of likelihood weighting on posterior location and spread in Bayesian meta-analysis estimated with JAGS and INLA. Bayesian Analysis (under review). https://arxiv.org/abs/2109.11870. 

Hunanyan, S. and Kazantzidis, G. and Rue, H. and Roos, M (2021). Empirical determinacy in the {B}ayesian logistic regression in the presence of separation in the data. (in preparation).
}
\keyword{ Empirical determinacy }
\examples{
#The effect of a coaching program on SAT-V (Scholastic Aptitude Test-Verbal) 
#scores in eight high schools, data from a randomized study were pre-analyzed and 
#supplied for a Bayesian meta-analysis. The eight schools data has been used to 
#study the performance of the Bayesian NNHM and to demonstrate typical issues, 
#which arise for BHMs (Gelman and Hill, 2007, Gelman et al., 2014, Vehtari et al
#., 2021]).

data(eight_schools)

#prior settings
mean_mu<-0
prec_mu<-1/(4^2) 
prec_tau<-1/(5^2)

#####################################
##########---NNHM in INLA---#########
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
                                data = eight_schools,
                                family = "gaussian",
                                scale = eight_schools$prec,
                                control.family = list(hyper=list(prec=list(initial = log(1), fixed=TRUE))),
                                control.fixed = list(mean.intercept=mean_mu, prec.intercept = prec_mu),
                                control.compute=list(hyperpar=TRUE),
                                num.threads=1)

del <- 0.01 #weighting factor w = 1 + del, w = 1 - del
#estimates the matrix of descriptive statistics given INLA base fit object
descriptives_inla_8schools <- extract_descriptives_mbp_inla(inla.obj = fit.inla.8schools, 
                              delta = del)
#estimates the empirical determinacy measure given the base INLA fit object 
ed_inla_8schools <- ed.inla(inla.object.base = fit.inla.8schools, delta = del, 
                            distance = "H2ALL")

#####################################
##########---NNHM in JAGS---#########
library(rjags)
library(coda)
#sessionInfo()
list.modules()
# In order to sample from the likelihood in JAGS one has to load the "dic" module and 
#mention "deviance" in variable.names
load.module("dic")
list.modules()

set.seed(44566)

###############
# rjags interface with code in a model string
###############
# In JAGS the normal distribution is parametrized by its mean and precision
jags_data<-list(y=eight_schools$y, sigma_2=eight_schools$prec, 
                mean_mu=mean_mu, prec_mu=prec_mu, 
                prec_tau=prec_tau)

# define parameters
schools.params.jags<-c("mu", "tau_2", "deviance")

schools_centered_modelString <- "
# likelihood
model{
for(i in 1:length(y)){
y[i] ~ dnorm(theta[i], sigma_2[i])
theta[i] ~ dnorm(mu, tau_2);
}

# priors
mu ~ dnorm(mean_mu, prec_mu);
tau ~ dnorm(0,prec_tau)T(0,);
tau_2 <- 1/(tau^2);
}
"

writeLines(schools_centered_modelString, con="TempModel.txt") # write to a file

# # model initiation
rjags.8schools  <- jags.model(
  file = "TempModel.txt",
  data = jags_data,
  n.chains = 4,
  n.adapt = 4000,
  inits = list( list(mu = 3.1, tau = 3.117181, 
                .RNG.name = "base::Wichmann-Hill", .RNG.seed = 314159),
                list(mu = 3.3, tau = 2.517181, 
                .RNG.name = "base::Marsaglia-Multicarry", .RNG.seed = 159314),
                list(mu = 3.6, tau = 3.317181, 
                .RNG.name = "base::Super-Duper", .RNG.seed = 413159),
                list(mu = 3.8, tau = 2.317181,  
                .RNG.name = "base::Mersenne-Twister", .RNG.seed = 143915)))

# burn-in
update(rjags.8schools , n.iter = 20000)
N.iter <- 2*(10^5)
n.thin <- 100
# sampling/monitoring
fit.rjags.8schools  <- coda.samples(
  model = rjags.8schools ,
  variable.names = schools.params.jags,
  n.iter = N.iter,
  thin = n.thin)
  
descriptives_jags_8schools <- extract_descriptives_mbp_jags(jags.obj = fit.rjags.8schools , 
                              prec.name = "tau_2", delta = del)

#empirical determinacy estimates
ed_jags_8schools <- ed.jags(jags.object.base=fit.rjags.8schools , delta = del, 
                            prec.name="tau_2", distance = "H2ALL")
                            
#Monte-Carlo standard error for empirical determinacy estimates

#mce_ed_8schools_jags <- mce.ed(object=fit.rjags.8schools, B=200, distance = "H2ALL",
#                          delta = del, prec.name="tau_2")

mce.ed(object=fit.rjags.8schools, B=200, distance = "H2ALL",
                          delta = del, prec.name="tau_2")

                          
#plot
#plot(mce_ed_8schools_jags, ranges = list("TED" = c(0, 0.5),
#                                "EDL" = c(0, 0.5),
#                                "EDS" = c(0, 0.5),
#                                "pEDL" = c(0, 1),
#                                "pEDS" = c(0, 1)))

############################################################
##########---Bayesian logisic regression in Stan---#########
data("bacterial_resistance")
df_ipm <- data.frame(cbind(bacterial_resistance$scIZD, bacterial_resistance$nipmesbl, 
                           bacterial_resistance$nipmwt))
colnames(df_ipm) <- c("scIZD", "nipmesbl", "nipmwt")

###################---Stan with binomial data---------########
params <- c("alpha","beta", "ll_gq")

stan_data_aggregated <- list(N=nrow(df_ipm), y =  df_ipm$nipmesbl, 
                             total = df_ipm$nipmesbl+df_ipm$nipmwt,
                             x1 = as.numeric(df_ipm$scIZD))


stan_inits <- list(list(alpha = -1.498195, beta = -0.3731706),
                   list(alpha = -1.398195, beta = -0.2731706),
                   list(alpha = -1.460195, beta = -0.3351706),
                   list(alpha = -1.436195, beta = -0.3111706))
library(rstan)

## Content of the Stan model
modelString = " // open quote for modelString
data {
int<lower=0> N;             // number of observations
int<lower=0> y[N];  // setting the dependent variable y as binomial
vector[N] x1;                // independent variable x1
int<lower=0> total[N]; // total observation for each x1
}

parameters {
real alpha; // logistic regression intercept
real beta; // logistic regression slope
}

model {
y ~ binomial_logit(total, alpha + beta*x1); 
// model for the logistic regression with a an aggregated binomial outcome
alpha ~ normal(0, 10);
beta ~ normal(0, 2.5);
}
generated quantities {
real ll_gq;
vector[N] log_lik;

for (n in 1:N) log_lik[n] = binomial_logit_lpmf(y[n] | total[n], alpha + 
                              x1[n] * beta );
ll_gq = sum(log_lik);
}
" # close quote for modelString

# Translate model to C++ and compile to DSO (dynamic shared object):
model_stanDSO <- stan_model(model_code = modelString)


fit.rstan_ipm <- sampling(
  object = model_stanDSO,
  data = stan_data_aggregated,
  pars = params,
  chains = 4,
  iter = 6*(10^3),
  warmup = 2*(10^3),
  thin = 100,
  seed = 1234,
  init = stan_inits)

fit.rstan_ipm_m <- as.matrix(fit.rstan_ipm)

#put the MCMC sample and the log-likelihood computed by hand together in a matrix
fit.rstan_ipm_m_ll <- as.matrix(cbind(fit.rstan_ipm_m[, c("alpha", "beta", "ll_gq")])) 

#change the name of the calumn containing the log-likelihood computed by
#the generated quantities block in Stan by ll
colnames(fit.rstan_ipm_m_ll) <- c("alpha", "beta", "ll") 

#the median and the 95\% CrIs of the posterior parameters
desc95_stan_ipm <- extract_median_95CrI_descriptives_stan(fit.rstan_ipm_m_ll, w = 1, 
                                                          prec.name = NULL)

#empirical determinacy estimates
ed_stan_ipm <- ed.stan(stan.object.base = fit.rstan_ipm_m_ll, 
                      distance = "H2ALL", delta = 0.01, prec.name=NULL)

#Monte Carlo standard error for empirical determinacy estimates
mce_ed_stan_ipm <- mce.ed(object = fit.rstan_ipm_m_ll, 
                                  B = 200, distance = "H2ALL", 
                                  delta = 0.01, prec.name = NULL)
                                
#plot
plot(mce_ed_stan_ipm, ranges = list("TED" = c(0, 0.5),
                                "EDL" = c(0, 0.5),
                                "EDS" = c(0, 0.5),
                                "pEDL" = c(0, 1),
                                "pEDS" = c(0, 1)))
}

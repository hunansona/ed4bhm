\name{plot.mce.ed}
\alias{plot.mce.ed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot of mce.ed 
}
\description{
Provides plots for the empirical determinacy estimates and their Monte Carlo standard errors.
}
\usage{
plot.mce.ed(object, ranges = NULL, title = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{mce.ed, an object of class mce.ed.}
  \item{ranges}{list, a named list of ranges for the limits of the y-axis of the plots, with names "TED", "EDL", "EDS", "pEDL", and "pEDS".}
  \item{title}{character, the title of the plot. }
}

\value{
Five plots for TED, EDL, EDS, pEDL, and pEDS estimates and their 95\% bootstrap confidence intervals. 
}

\author{
Georgios Kazantzidis, Malgorzata Roos
}
\examples{
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


stan_sim.ipm.aggregated <- sampling(
  object = model_stanDSO,
  data = stan_data_aggregated,
  pars = params,
  chains = 4,
  iter = 6*(10^3),
  warmup = 2*(10^3),
  thin = 100,
  seed = 1234,
  init = stan_inits)

fit.rstan_ipm.aggregated_logit_m <- as.matrix(stan_sim.ipm.aggregated)

# we put the MCMC sample and the log-likelihood computed by hand together in a matrix
fit.rstan_ipm.aggregated_logit_m_ll <- as.matrix(cbind(fit.rstan_ipm.aggregated_logit_m[,c("alpha", "beta", "ll_gq")])) 

# we change the name of the calumn containing the log-likelihood computed by generated quantities to ll
colnames(fit.rstan_ipm.aggregated_logit_m_ll) <- c("alpha", "beta", "ll") 


#Monte Carlo standard error for empirical determinacy estimates
mce_ed_stan_ipm_ll <- mce.ed(object = fit.rstan_ipm.aggregated_logit_m_ll, 
                                  B = 200, distance = "H2ALL", 
                                  delta = 0.01, prec.name = NULL)
                                  
plot(mce_ed_stan_ipm_ll, ranges = list("TED" = c(0, 0.5),
                                "EDL" = c(0, 0.5),
                                "EDS" = c(0, 0.5),
                                "pEDL" = c(0, 1),
                                "pEDS" = c(0, 1)) )


}
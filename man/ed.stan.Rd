\name{ed.stan}
\alias{ed.stan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Empirical determinacy for Stan
}
\description{Generic function. Provides empirical determinacy estimates TED, EDL, EDS, pEDL, and pEDS from a Stan model. This function takes the Stan fit object from the base model as input and returns the identification values, descritive.matrix and delta.
}
\usage{
ed.stan(stan.object.base,
                    prec.name = NULL, 
                    delta = 0.01,
                    distance = "H2ALL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stan.object.base}{Stan fit object from the base model with a column called "ll", which contains the log-likelihood evaluated at MCMC sample of parameters.}
  \item{prec.name}{NULL or character, specifies the names of parameters (in JAGS and INLA precisions but in Stan standard deviations) to be log-transformed}
  \item{delta}{numeric, the numerical differentiation step and \eqn{w = 1 \pm \delta} is the weight, default value is 0.01.}
  \item{distance}{character string, specifies the type of the empirical determinacy measure. It can have values "H2" for TED, "BCL" for EDL and "BCS" for EDS and "H2ALL" for all the five measures (TED, EDL, EDS, pEDL and pEDS). The default value is "H2ALL".}
}

\value{
\item{H2ALL measures}{a matrix of identification values}
\item{descriptive matrix}{a list of descriptive matrix and \eqn{\delta}}
}

\author{
Malgorzata Roos, Georgios Kazantzidis, Sona Hunanyan
}
\references{Hunanyan, S. and Kazantzidis, G. and Rue, H. and Roos, M (2021). Empirical determinacy in the {B}ayesian logistic regression in the presence of separation in the data. (in preparation).
%% ~put references to the literature/web site here ~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{ed}}, package \code{\link{rstan}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
############################################################
##########---Bayesian logisic regression in Stan---#########
data("bacterial_resistance")
df_ipm <- data.frame(cbind(bacterial_resistance$scIZD, 
                            bacterial_resistance$nipmesbl, 
                            bacterial_resistance$nipmwt))
colnames(df_ipm) <- c("scIZD", "nipmesbl", "nipmwt")

###################---Stan with binomial data---------########
params <- c("alpha","beta", "ll_gq")

stan_data <- list(N=nrow(df_ipm), y =  df_ipm$nipmesbl, 
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


stan_ipm <- sampling(
  object = model_stanDSO,
  data = stan_data,
  pars = params,
  chains = 4,
  iter = 6*(10^3),
  warmup = 2*(10^3),
  thin = 100,
  seed = 1234,
  init = stan_inits)

fit.rstan_ipm_m <- as.matrix(stan_ipm)

# we put the MCMC sample and the log-likelihood computed 
# by hand together in a matrix
fit.rstan_ipm_m_ll <- as.matrix(cbind(fit.rstan_ipm_m[,c("alpha", 
                                        "beta", "ll_gq")])) 

# we change the name of the calumn containing the log-likelihood 
# computed by generated quantities in Stan by ll
colnames(fit.rstan_ipm_m_ll) <- c("alpha", "beta", "ll") 

#the median and the 95\% CrIs of the posterior parameters
descriptives95_stan_ipm <- extract_median_95CrI_descriptives_stan(
                              fit.rstan_ipm_m_ll, 
                              w = 1, prec.name = NULL)

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
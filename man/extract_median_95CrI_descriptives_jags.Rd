\name{extract_median_95CrI_descriptives_jags}
\alias{extract_median_95CrI_descriptives_jags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of the median and the equi-tailed 95\% CrI from a JAGS object
%%  ~~function to do ... ~~
}
\description{This function takes a JAGS object as input and extracts the median and the equi-tailed 95\% CrI of all the model parameters (without random effects).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_median_95CrI_descriptives_jags(jags.object.base, w, prec.name)
}
\arguments{
  \item{jags.object.base}{character string, the name of the JAGS object fitted in \code{rjags} 
}
  \item{w}{numeric, the weighting factor to weight the likelihood. For the base model \eqn{w=1}.
}
  \item{prec.name}{character string, the name of the precision parameter to be used for logarithmic transformation of the marginal posterior samples of the precision parameter.
}
}
\details{The returned matrix has three columns with names "0.025quant", "0.5quant" and "0.975quant" for the median and the equi-tailed 95\% CrI of the marginal posterior distributions and has as many rows as there are parameters (without random effects) in the model.
}
\value{numerical matix
}
\seealso{\code{\link{rjags}}
}
\examples{
data(eight_schools)
#prior settings
mean_mu<-0
prec_mu<-1/(4^2) 
prec_tau<-1/(5^2)

##########################################################
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
jags_data <- list(y=eight_schools$y, sigma_2=eight_schools$prec, 
                mean_mu=mean_mu, prec_mu=prec_mu, 
                prec_tau=prec_tau)

# define parameters
schools.params.jags <- c("mu", "tau_2", "deviance")

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
  inits = list(list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 44561),
               list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 44562),
               list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 44563),
               list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 44564)))

# burn-in
update(rjags.8schools, n.iter = 20000)
N.iter <- 2*(10^5)
n.thin <- 100
# sampling/monitoring
fit.rjags.8schools <- coda.samples(
  model = rjags.8schools ,
  variable.names = schools.params.jags,
  n.iter = N.iter,
  thin = n.thin)

descriptives_median_95CrI_jags_8schools <- extract_median_95CrI_descriptives_jags(jags.obj = 
                                            fit.rjags.8schools, 
                                            w = 1, prec.name = "tau_2")
}

\name{extract_descriptives_mbp_jags}
\alias{extract_descriptives_mbp_jags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of descriptive statistics from a JAGS object for base and weighted models
%%  ~~function to do ... ~~
}
\description{This function takes a JAGS object as input and extracts the descriptive statistics (mean and standard deviation) of all the model parameters (without random effects) for the base and weighted models.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_descriptives_mbp_jags(jags.object.base, delta, prec.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jags.object.base}{character string, the name of the JAGS object fitted in \code{rjags} with the base model (non-weighted likelihood, \eqn{w = 1})
%%     ~~Describe \code{jags.object.base} here~~
}
  \item{delta}{numeric, numerical differentiation step, the weighting factor \eqn{w = 1 \pm \delta}, the default value is 0.01.
%%     ~~Describe \code{delta} here~~
}
  \item{prec.name}{character string, the name of the precision parameter to be used for logarithmic transformation of the posterior sampples for the precision parameter.
%%     ~~Describe \code{prec.name} here~~
}
}
\details{The numerical value represents \eqn{\delta}, the matrix has six columns with names ("mean.minus", "sd.minus", "mean.base", "sd.base", "mean.plus", "sd.plus") for the mean and standard deviation of the marginal posterior distributions from the models with \eqn{w = 1 - 0.01}, \eqn{w = 1} (base) and \eqn{w = 1 + 0.01}, and has as many rows as there are parameters in the model (without random effects).
%%  ~~ If necessary, more details than the description above ~~
}
\value{list composed of a numerical matix and a numerical value 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{extract_descriptives_jags}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
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

del = 0.01

descriptives_jags_8schools <- extract_descriptives_mbp_jags(jags.obj = fit.rjags.8schools, 
                                                          prec.name = "tau_2", delta = del)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

\name{ed.jags}
\alias{ed.jags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Empirical determinacy for JAGS
%%  ~~function to do ... ~~
}
\description{Calculates the total empirical determinacy (TED) and proportional empirical determinacy for location (pEDL) and for spread (pEDS) for JAGS.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ed.jags(jags.object.base, distance = "H2ALL", delta = 0.01, prec.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jags.object.base}{character string, the name of the JAGS object fitted with \code{rjags} with the base model (non-weighted likelihood)
%%     ~~Describe \code{jags.object.base} here~~
}
  \item{distance}{character string, specifies the type of the empirical determinacy measure. It can have values "H2" for TED, "BCL" for EDL and "BCS" for EDS and "H2ALL" for all the five measures (TED, EDL, EDS, pEDL and pEDS). The default value is "H2ALL".
%%     ~~Describe \code{distance} here~~
}
  \item{delta}{numeric, numerical differentiation step, the weighting factor \eqn{w = 1 \pm \delta}, the default value is 0.01.
%%     ~~Describe \code{delta} here~~
}
  \item{prec.name}{character string, the name of the precision parameter to be used for logarithmic transformation
%%     ~~Describe \code{prec.name} here~~
}
}
\details{The first element in the final returned value does not depend on the value of \code{distance}. The argument \code{distance} = "H2ALL" provides a list, where the second element of the list is a matrix of five empirical determinacy measures. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{A list of length two. The first element of the list is a list containing the matrix of descriptive statistics (mean and standard deviation) for each parameter in the model and the value of \eqn{\delta}. The second element of the list is a matrix of empirical determinacy measures for each parameter in the model. This matrix has columns \code{c("TED", "EDL", "EDS", "pEDL", "pEDS")} when \code{distance = "H2ALL"}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Hunanyan, S., Roos, M., Plummer, M., Rue, H. (2021). Quantification of empirical determinacy: the impact of likelihood weighting on posterior location and spread in Bayesian meta-analysis estimated with JAGS and INLA. \emph{Bayesian Analaysis} (in preparation)
%% ~put references to the literature/web site here ~
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{ed}}, package \code{\link{rjags}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(eight_schools)
#prior settings
mean_mu<-0
prec_mu<-1/(4^2) 
prec_tau<-1/(5^2)

library(rjags)
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
update(rjags.8schools , n.iter = 20000)
N.iter <- 2*(10^5)
n.thin <- 100
# sampling/monitoring
fit.rjags.8schools  <- coda.samples(
  model = rjags.8schools ,
  variable.names = schools.params.jags,
  n.iter = N.iter,
  thin = n.thin)

}

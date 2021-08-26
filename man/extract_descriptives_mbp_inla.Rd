\name{extract_descriptives_mbp_inla}
\alias{extract_descriptives_mbp_inla}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of descriptive statistics from an INLA object for base and weighted models
%%  ~~function to do ... ~~
}
\description{This function takes an INLA object as input and extracts the descriptive statistics (mean and standard deviation) of all the model parameters for the base and weighted models.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_descriptives_mbp_inla(inla.object.base, delta, dz, diff.logdens)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object.base}{character string, the name of the INLA object fitted in R-INLA with the base model (non-weighted likelihood, \eqn{w = 1})
%%     ~~Describe \code{inla.object.base} here~~
}
  \item{delta}{numeric, numerical differentiation step, the weighting factor \eqn{w = 1 \pm \delta}, the default value is 0.01.
%%     ~~Describe \code{delta} here~~
}
  \item{dz}{Step length in the standardized scale used in the construction of the grid, default 0.75.
%%     ~~Describe \code{dz} here~~
}
  \item{diff.logdens}{The difference of the log.density for the hyperpameters to stop numerical integration using int.strategy='grid'. Default 15.
%%     ~~Describe \code{diff.logdens} here~~
}
}
\details{The numerical value represents \eqn{\delta}, the matrix has six columns with names ("mean.minus", "sd.minus", "mean.base", "sd.base", "mean.plus", "sd.plus") for the mean and standard deviation of the marginal posterior distributions from the models with \eqn{w = 1 - 0.01}, \eqn{w = 1} and \eqn{w = 1 + 0.01}, and has as many rows as there are parameters in the model.}

\value{list composed of a numerical matix and a numerical value 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{extract_descriptives_inla}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{data(eight_schools)
#prior settings
mean_mu<-0
prec_mu<-1/(4^2) # Stan assumes sd=5 -> precision is equal to 1/(5^2)=0.04
prec_tau<-1/(5^2)# distribution that is 

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

# INLA uses the centered parametrization of the 8 schools model by default
fit.inla.8schools <- inla(formula.8schools.HN,
                                data = eight_schools,
                                family = "gaussian",
                                scale = eight_schools$prec,
                                control.family = list(hyper=list(prec=list(initial = log(1), fixed=TRUE))),
                                control.fixed = list(mean.intercept=mean_mu, prec.intercept=prec_mu),
                                control.compute=list(hyperpar=TRUE),
                                num.threads=1)
                                
del <- 0.01
descriptives_inla_8schools <- extract_descriptives_mbp_inla(inla.obj = fit.inla.8schools, delta = del)
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

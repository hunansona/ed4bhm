\name{extract_descriptives_inla}
\alias{extract_descriptives_inla}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of descriptive statistics from an INLA object
%%  ~~function to do ... ~~
}
\description{This function takes an INLA object as input and extracts the descriptive statistics (mean and standard deviation) of all the model parameters.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_descriptives_inla(inla.object, dz = 0.75, diff.logdens = 15)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object}{character string, the name of the INLA object fitted in \code{inla}
%%     ~~Describe \code{inla.object} here~~
}
  \item{dz}{Step length in the standardized scale used in the construction of the grid, default 0.75.
%%     ~~Describe \code{dz} here~~
}
  \item{diff.logdens}{The difference of the log.density for the hyperpameters to stop numerical integration using int.strategy='grid'. Default 15.
%%     ~~Describe \code{diff.logdens} here~~
}
}

\details{The returned matrix has two columns with names "mean" and "sd" for the mean and standard deviation of the marginal posterior distributions and has as many rows as there are parameters in the model.
}
\value{numerical matix
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{inla}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(eight_schools)
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

descriptives_inla_8schools <- extract_descriptives_inla(inla.obj = fit.inla.8schools)
}

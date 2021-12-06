\name{ed.inla}
\alias{ed.inla}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Empirical determinacy for INLA
%%  ~~function to do ... ~~
}
\description{Calculates the total empirical determinacy (TED) and proportional empirical determinacy for location (pEDL) and for spread (pEDS) for INLA.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ed.inla(inla.object.base, distance = "H2ALL", dz = 0.75, diff.logdens = 15, delta = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object.base}{character string, the name of the INLA object fitted in R-INLA with the base model (non-weighted likelihood)
%%     ~~Describe \code{inla.object.base} here~~
}
  \item{distance}{character string, specifies the type of the empirical determinacy measure. It can have values "H2" for TED, "BCL" for EDL and "BCS" for EDS and "H2ALL" for all the five measures (TED, EDL, EDS, pEDL and pEDS). The default value is "H2ALL".
%%     ~~Describe \code{distance} here~~
}
  \item{dz}{Step length in the standardized scale used in the construction of the grid, default 0.75.
%%     ~~Describe \code{dz} here~~
}
  \item{diff.logdens}{The difference of the log.density for the hyperpameters to stop numerical integration using \code{int.strategy='grid'}. Default 15.
%%     ~~Describe \code{diff.logdens} here~~
}
  \item{delta}{numeric, numerical differentiation step, the weighting factor \eqn{w = 1 \pm \delta}, the default value is 0.01.
%%     ~~Describe \code{delta} here~~
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
\references{Hunanyan, S., Roos, M., Plummer, M., Rue, H. (2021). Quantification of empirical determinacy: the impact of likelihood weighting on posterior location and spread in Bayesian meta-analysis estimated with JAGS and INLA. \emph{Bayesian Analaysis} (under review). https://arxiv.org/abs/2109.11870.
%% ~put references to the literature/web site here ~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{ed}}, package \code{\link{inla}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
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
ed_inla_8schools <- ed.inla(inla.object.base = fit.inla.8schools, delta = del, dz = 0.75, 
                            diff.logdens = 15, distance = "H2ALL")
}
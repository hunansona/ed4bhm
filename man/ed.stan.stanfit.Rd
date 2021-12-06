\name{ed.stan.stanfit}
\alias{ed.stan.stanfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Empirical determinacy estimates for stan
}
\description{
Provides empirical determinacy estimates TED, EDL, EDS, pEDL, and pEDS from a stan model. This function takes the stan fit object from the base model as input and returns the identification values, descritive.matrix and delta.
}
\usage{
ed.stan.stanfit(stan.object.base,
                    prec.name = NULL, 
                    delta = 0.01,
                    distance = "H2ALL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stan.object.base}{stan fit object from the base model with a column called "ll", which contains the log-likelihood evaluated at MCMC sample of parameters.
  }
  \item{prec.name}{NULL or character, specifies the names of parameters (in JAGS and INLA precisions but in Stan standard deviations) to be log-transformed}
  \item{delta}{numeric, the numerical differentiation step and \eqn{w = 1 \pm \delta} is the weight, default value is 0.01.}
  \item{distance}{character string, specifies the type of the empirical determinacy measure. It can have values "H2" for TED, "BCL" for EDL and "BCS" for EDS and "H2ALL" for all the five measures (TED, EDL, EDS, pEDL and pEDS). The default value is "H2ALL".}
}
\value{
\item{H2ALL measures}{a matrix of identification values }
\item{descriptive matrix }{A list of descriptive matrix and delta}
}
\author{
Malgorzata Roos, Georgios Kazantzidis, Sona Hunanyan,
}

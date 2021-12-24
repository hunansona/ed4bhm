\name{extract_descriptives_mbp_stan}
\alias{extract_descriptives_mbp_stan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of descriptive statistics from a Stan object for base and weighted models
}
\description{
This function applies weighting to the stan.object.base.modified
}
\usage{
extract_descriptives_mbp_stan(stan.object.base.modified, prec.name, delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stan.object.base.modified}{a Stan object from the base model in matrix form with a column called ll, which contains the log-likelihood evaluated at MCMC sample of parameters.}
  \item{prec.name}{NULL or character, specifies the names of parameters (in JAGS and INLA precisions but in Stan standard deviations) for the log-transformation}
  \item{delta}{numeric, numerical differentiation step, the weighting factor \eqn{w = 1 \pm \delta}, the default value is 0.01.}  
}
\value{
A list of the descriptive matrix (means and sds for minus, base, plus models) and the delta 
}
\author{
Malgorzata Roos, Georgios Kazantzidis, Sona Hunanyan,
}

\name{mce.ed}
\alias{mce.ed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ 
Monte Carlo Error for empirical determinacy estimates
}
\description{ 
Generic fucniton. Estimates the Monte Carlo standard error for empirical determinacy estimates. 
}
\usage{
mce.ed(object, B = 200, rseed = 458275, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{mcmc.list} or \code{stanfit} or \code{matrix}.}
  \item{B}{numeric, the number of bootstrap replications.}
  \item{rseed}{numeric, the random seed number.}
  \item{...}{Arguments to be passed to methods}
}
\value{
A matrix with the empirical determinacy estimates and their Monte Carlo standard errors.
}

\author{
Georgios Kazantzidis, Sona Hunanyan, Malgorzata Roos 
}


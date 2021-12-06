\name{mce.ed.matrix}
\alias{mce.ed.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ 
Monte Carlo Error for empirical determinacy estimates
}
\description{ 
Estimates the Monte Carlo standard error for empirical determinacy estimates. 
}
\usage{
mce.ed.matrix(object, B = 200, rseed = 458275, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class matrix.}
  \item{B}{numeric The number of bootstrap replications to be used.}
  \item{rseed}{numeric The seed number.}
  \item{...}{Additional arguments for \code{\link{ed.stan}}}
}
\value{

A matrix with the empirical determinacy estimates and their Monte Carlo standard errors.

}
\author{
Georgios Kazantzidis, Sona Hunanyan, Malgorzata Roos 
}
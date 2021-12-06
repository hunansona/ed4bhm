\name{extract_descriptives_stan}
\alias{extract_descriptives_stan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of descriptive statistics from a Stan object
}
\description{
This function takes a stan object and extracts the descriptive statistics (mean and standard deviation) of the model parameters
}
\usage{
extract_descriptives_stan(stan.object.base.modified, w, prec.name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stan.object.base.modified}{A stan object in matrix form with a column called ll, which contains the log-likelihood evaluated at MCMC sample of parameters }
  \item{w}{numeric, the weighting factor to weight the likelihood. For the base model \eqn{w=1}.}
  \item{prec.name}{NULL or character, specifies the names of parameters (in JAGS and INLA precisions but in Stan standard deviations) for transformation to the log-scale}
}

\value{
A descriptive matrix with two columns (mean and sd) and rows for all the parameters of the model
}

\author{
Malgorzata Roos, Georgios Kazantzidis, Sona Hunanyan,
}

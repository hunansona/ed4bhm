\name{ed}
\alias{ed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Empirical determinacy
%%  ~~function to do ... ~~
}
\description{A vector of empirical determinacy values for all the parameters in the model for the given distance measure
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ed(descriptive.matrix.ls, distance = "H2ALL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{descriptive.matrix.ls}{a list, that contains descriptive matrix and delta, where the descriptive matrix must have the following six columns with names ("mean_plus", "sd_plus", "mean_base", "sd_base", "mean_minus", "sd_minus") and \eqn{\delta} represents the numerical differentiation step 
%%     ~~Describe \code{descriptive.matrix.ls} here~~
}
  \item{distance}{character string, specifies the type of the empirical determinacy measure. It can have values "H2" for TED, "BCL" for EDL and "BCS" for EDS and "H2ALL" for all the five measures (TED, EDL, EDS, pEDL and pEDS). The default value is "H2ALL".
%%     ~~Describe \code{distance} here~~
}
}
\value{a numeric vector of empirical determinacy values for the parameters in the model for the given distance measure.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{extract_descriptives_mbp_inla}}, \code{\link{extract_descriptives_mbp_jags}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
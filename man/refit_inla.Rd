\name{refit_inla}
\alias{refit_inla}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{This function takes the INLA fitted object from the base model as input and fits weighted INLA models with weights \eqn{w = 1 - \delta} and \eqn{w = 1 + \delta}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
refit_inla(inla.object.base, delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object.base}{character string, the name of the INLA object fitted in R-INLA from the base (non-weighted) model
%%     ~~Describe \code{inla.object.base} here~~
}
  \item{delta}{numeric, numerical differentiation step, the weighting factor w = 1 \pm delta, the default value is 0.01.
%%     ~~Describe \code{delta} here~~
}
}
\details{To access the \code{weights} used in INLA fit use \code{inla.object.base\$.args\$weights}. The function \code{refit_inla} modifies the original model given by \code{inla.object.base} once with \code{weigts = 1-\delta} and then with \code{weigts = 1-\delta} and fits the modified models again in INLA.
%%  ~~ If necessary, more details than the description above ~~
}
\value{list composed of two INLA objects
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
\note{Read more details regarding the \code{weights} argument in \code{\link{inla}}.
}
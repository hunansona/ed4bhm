\name{d2_interface_vector}
\alias{d2_interface_vector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Numerical approximation of the second derivative
%%  ~~function to do ... ~~
}
\description{Calculates the numerical approximation of the second derivative of empirical determinacy measures for the given distance measures. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
d2_interface_vector(avector, delta, method)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{avector}{a numeric vector of the means and standard deviations for the base and weighted models with normal densities as posterior distrubutions. This vector has the structure c(mam, sdam, mb, sdb, map, sdap), where \code{mam} and \code{sdam} are the mean and the standard deviation from the model with likelihood weighted by a factor of \deqn{w = 1 - \delta}, \code{mb} and {sdb} are the mean and standard deviation from the base model and \code{map} and \code{sdap} are the mean and standard deviation from the model with likelihood weighted by a factor of \deqn{w = 1 + \delta}.
%%     ~~Describe \code{avector} here~~
}
  \item{delta}{numeric, numerical differentiation step, the weighting factor w = 1 \pm delta, the default value is 0.01
%%     ~~Describe \code{delta} here~~
}
  \item{method}{character string, specifies the type of the distance 
%%     ~~Describe \code{method} here~~
}
}
\details{A numerical approximation of the second derivative
%%  ~~ If necessary, more details than the description above ~~
}
\value{A numerical value of the numerical approximation of second derivative by second-order central difference quotient formula.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
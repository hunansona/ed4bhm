\name{d2_central_difference}
\alias{d2_central_difference}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Numerical approximation of the second derivative
%%  ~~function to do ... ~~
}
\description{Calculates the numerical approximation of the second derivative of empirical determinacy measures expressed by Bhattacharyya coefficient (BC) for the given method
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
d2_central_difference(desc_vector, method, delta = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{desc_vector}{a numeric vector of the means and standard deviations for the base and weighted models with normal posterior distrubutions. This vector has the structure c(\code{mwm}, \code{sdwm}, \code{mb}, \code{sdb}, \code{mwp}, \code{sdwp}), where \code{mwm} and \code{sdwm} are the mean and the standard deviation from the model with likelihood weighted by a factor of \eqn{w = 1 - \delta}, \code{mb} and {sdb} are the mean and standard deviation from the base model and \code{mwp} and \code{sdwp} are the mean and standard deviation from the model with likelihood weighted by a factor of \eqn{w = 1 + \delta}.
%%     ~~Describe \code{desc_vector} here~~
}
  \item{method}{character string, specifies the type of the distance, which can attain values "H2", "BCL", "BCS".
%%     ~~Describe \code{method} here~~
}
  \item{delta}{numeric, numerical differentiation step, for which the weighting factor \eqn{w = 1 \pm \delta}. Default value is 0.01
%%     ~~Describe \code{delta} here~~
}

}
\details{This function approximates the second derivative of the corresponding distance measure by second-order central difference quotient formula. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{A numerical value of the numerical approximation of second derivative.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
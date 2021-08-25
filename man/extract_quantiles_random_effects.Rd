\name{extract_quantiles_random_effects}
\alias{extract_quantiles_random_effects}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of quantiles for the random effects in INLA
%%  ~~function to do ... ~~
}
\description{This function takes an INLA object as input and extracts the quantiles (2.5\%, 50\%, 97.5\%) of the random effects.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_quantiles_random_effects(inla.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object}{character string, the name of the INLA object fitted in R-INLA
%%     ~~Describe \code{inla.object} here~~
}
}
\details{This function takes an \code{inla} object as input, checks whether the random effects terms are available and extracts the quantiles of the random effects.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A matrix of numeric values for all the random effects in rows and quantiles in columns (ncol = 3).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\seealso{\code{\link{extract_quantiles_descriptives_inla}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
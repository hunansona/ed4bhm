\name{extract_quantiles_fixed_effects}
\alias{extract_quantiles_fixed_effects}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of quantiles for the fixed effects in INLA
%%  ~~function to do ... ~~
}
\description{This function takes an INLA object as input and extracts the quantiles (2.5\%, 50\%, 97.5\%) of the fixed effect terms.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_quantiles_fixed_effects(inla.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object}{character string, the name of the INLA object fitted in \code{inla}
%%     ~~Describe \code{inla.object} here~~
}
}
\details{This function takes an \code{inla} object as input, checks whether the fixed effects terms are available and extracts the corresponding quantiles of the fixed effects parameters.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A matrix of numeric values for all the fixed effects parameters in rows and quantiles in columns (ncol = 3).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\seealso{\code{\link{extract_quantiles_descriptives_inla}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
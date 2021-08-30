\name{extract_fixed_effects}
\alias{extract_fixed_effects}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of descriptive statistics for the fixed effects in INLA
%%  ~~function to do ... ~~
}
\description{This function takes an INLA object as input and extracts the descriptive statistics (mean and standard deviation) of the fixed effect terms.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_fixed_effects(inla.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object}{character string, the name of the INLA object fitted in \code{inla}
%%     ~~Describe \code{inla.object} here~~
}
}
\details{This function takes an \code{inla} object as input, checks whether the fixed effects terms are available in the model and extracts the descriptive statistics (mean and standard deviation) of the fixed effects parameters.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A matrix of numeric values for all the fixed effects parameters in rows and means and standard deviations in columns (ncol = 2).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{extract_descriptives_inla}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\name{extract_random_effects}
\alias{extract_random_effects}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of descriptive statistics for the random effects in INLA
%%  ~~function to do ... ~~
}
\description{This function takes an INLA object as input and extracts the descriptive statistics (mean and standard deviation) of the random effects.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_random_effects(inla.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object}{character string, the name of the INLA object fitted in \code{inla}
%%     ~~Describe \code{inla.object} here~~
}
}
\details{This function takes an \code{inla} object as input, checks whether the random effects are available and extracts the descriptive statistics (mean and standard deviation) of the random effects.
}
\value{A matrix of numeric values for all the random effects in rows and means and sds in columns (ncol = 2).
}

\seealso{\code{\link{extract_descriptives_inla}}
}
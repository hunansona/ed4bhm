\name{extract_hyperparameters}
\alias{extract_hyperparameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of descriptive statistics for the hyperparameters in INLA
%%  ~~function to do ... ~~
}
\description{This function takes an INLA object as input and extracts the descriptive statistics (mean and standard deviation) of the hyperparameters.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_hyperparameters(inla.object, dz = 0.75, diff.logdens = 15)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object}{character string, the name of the INLA object fitted in R-INLA
%%     ~~Describe \code{inla.object} here~~
}
  \item{dz}{Step length in the standardized scale used in the construction of the grid, default 0.75.
%%     ~~Describe \code{dz} here~~
}
  \item{diff.logdens}{The difference of the log.density for the hyperpameters to stop numerical integration using int.strategy='grid'. Default 15.
}
}
\details{This function takes an \code{inla} object as input, improves the estimates of the posterior marginals for the hyperparameters of the model using the grid integration strategy by \code{inla.hyperpar} function, checks whether the hyperparamaters are available and extracts the descriptive statistics (mean and standard deviation) of the hyperparameters.
}
\value{A matrix of numeric values for all the hyperparameters in rows and means and sds in columns (ncol = 2).
}

\seealso{\code{\link{extract_descriptives_inla}}
}

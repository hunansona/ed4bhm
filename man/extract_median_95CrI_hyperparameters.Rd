\name{extract_median_95CrI_hyperparameters}
\alias{extract_median_95CrI_hyperparameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of the median and the equi-tailed 95\% CrI for the hyperparameters in INLA
%%  ~~function to do ... ~~
}
\description{This function takes an INLA object as input and extracts the median and the equi-tailed 95\% CrI of the hyperparameters.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_median_95CrI_hyperparameters(inla.object, dz = 0.75, diff.logdens = 15)
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
%%     ~~Describe \code{diff.logdens} here~~
}
}
\details{This function takes an \code{inla} object as input, checks whether the hyperparameters are available and extracts the median and the equi-tailed 95\% CrI of the hyperparameters.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A matrix of numeric values for all the hyperparameters in rows and median and the equi-tailed 95\% CrI in columns (ncol = 3).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\seealso{\code{\link{extract_median_95CrI_descriptives_inla}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
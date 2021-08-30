\name{ndat}
\alias{ndat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Number of input data points
%%  ~~function to do ... ~~
}
\description{This is a helper function, which extracts the number of data points given an INLA object
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ndat(inla.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object}{character string, the name of the INLA object fitted in \code{inla}
%%     ~~Describe \code{inla.object} here~~
}
}

\value{numeric, the number of observations in the dataset used for fitting the model the result of which is given by \code{inla.object}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\seealso{\code{\link{inla}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
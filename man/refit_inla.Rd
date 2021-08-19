\name{refit_inla}
\alias{refit_inla}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
refit_inla(inla.object.base, delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object.base}{
%%     ~~Describe \code{inla.object.base} here~~
}
  \item{delta}{
%%     ~~Describe \code{delta} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (inla.object.base, delta) 
{
    numdat <- ndat(inla.object.base)
    result_m <- inla.object.base
    result_p <- inla.object.base
    result_m$.args$weights <- rep(1 - delta, numdat)
    result_p$.args$weights <- rep(1 + delta, numdat)
    inla.setOption(enable.inla.argument.weights = TRUE)
    inla.object.altered.p <- inla.rerun(result_p, plain = TRUE)
    inla.object.altered.m <- inla.rerun(result_m, plain = TRUE)
    return(list(inla.object.altered.plus = inla.object.altered.p, 
        inla.object.altered.minus = inla.object.altered.m))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

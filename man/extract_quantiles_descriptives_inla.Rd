\name{extract_quantiles_descriptives_inla}
\alias{extract_quantiles_descriptives_inla}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_quantiles_descriptives_inla(inla.object, dz, diff.logdens)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object}{
%%     ~~Describe \code{inla.object} here~~
}
  \item{dz}{
%%     ~~Describe \code{dz} here~~
}
  \item{diff.logdens}{
%%     ~~Describe \code{diff.logdens} here~~
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
function (inla.object, dz, diff.logdens) 
{
    descriptive.matrix <- NULL
    fixed.matrix <- extract_quantiles_fixed_effects(inla.object)
    hyperpar.matrix <- extract_quantiles_hyperpars(inla.object, 
        dz, diff.logdens)
    random.matrix <- extract_quantiles_random_effects(inla.object)
    if (!is.null(fixed.matrix)) {
        descriptive.matrix <- fixed.matrix
    }
    if (!is.null(hyperpar.matrix)) {
        descriptive.matrix <- rbind(descriptive.matrix, hyperpar.matrix)
    }
    if (!is.null(random.matrix)) {
        descriptive.matrix <- rbind(descriptive.matrix, random.matrix)
    }
    colnames(descriptive.matrix) <- c("0.025quant", "0.5quant", 
        "0.975quant")
    namere <- names(inla.object$summary.random)
    return(descriptive.matrix)
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

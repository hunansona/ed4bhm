\name{extract_quantiles_random_effects}
\alias{extract_quantiles_random_effects}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_quantiles_random_effects(inla.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inla.object}{
%%     ~~Describe \code{inla.object} here~~
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
function (inla.object) 
{
    random.matrix <- NULL
    if (length(inla.object$summary.random) == 1) {
        random.length <- length(inla.object$summary.random)
        namere <- names(inla.object$summary.random)
        random.matrix <- rbind(random.matrix, cbind(unlist(inla.object$summary.random[[1]][, 
            "0.025quant"], use.names = TRUE), unlist(inla.object$summary.random[[1]][, 
            "0.5quant"], use.names = TRUE), unlist(inla.object$summary.random[[1]][, 
            "0.975quant"], use.names = TRUE)))
        nre <- nrow(inla.object$summary.random[[1]])
        row.names(random.matrix) <- paste(namere[1], 1:nre, sep = "")
    }
    else if (length(inla.object$summary.random) >= 2) {
        for (ii in 2:random.length) {
            random.matrix.i <- cbind(unlist(inla.object$summary.random[[ii]][, 
                "0.025quant"]), unlist(inla.object$summary.random[[ii]][, 
                "0.5quant"]), unlist(inla.object$summary.random[[ii]][, 
                "0.975quant"], use.names = TRUE))
            nre <- nrow(inla.object$summary.random[[ii]])
            row.names(random.matrix.i) <- paste(namere[ii], 1:nre, 
                sep = "")
            random.matrix <- rbind(random.matrix, random.matrix.i)
        }
    }
    return(random.matrix)
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

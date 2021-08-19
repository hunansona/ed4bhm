\name{extract_quantiles_hyperpars}
\alias{extract_quantiles_hyperpars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_quantiles_hyperpars(inla.object, dz = 0.75, diff.logdens = 15)
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
function (inla.object, dz = 0.75, diff.logdens = 15) 
{
    hyperpar.matrix <- NULL
    if (!is.null(inla.object$summary.hyperpar)) {
        hyps <- inla.object$internal.summary.hyperpar
        hyperpar.list <- cbind(hyps["0.025quant"], hyps["0.5quant"], 
            hyps["0.975quant"])
        hyperpar.matrix <- matrix(unlist(hyperpar.list), ncol = 3)
        row.names(hyperpar.matrix) <- row.names(hyps)
    }
    return(hyperpar.matrix)
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

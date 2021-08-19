\name{weightFC_jags}
\alias{weightFC_jags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weightFC_jags(jags.obj, w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jags.obj}{
%%     ~~Describe \code{jags.obj} here~~
}
  \item{w}{
%%     ~~Describe \code{w} here~~
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
function (jags.obj, w) 
{
    if (is.null(jags.obj[, "deviance"])) {
        cat("run the jags model again to get also the deviance sample. To get the deviance sample in jags, load the module dic and add the deviance in the jags parameter list")
    }
    else {
        deviance_sample <- unlist(jags.obj[, "deviance"])
    }
    probs_i_sample <- exp(-((w - 1) * deviance_sample)/2)
    return(probs_i_sample)
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

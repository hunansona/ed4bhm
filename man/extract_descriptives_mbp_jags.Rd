\name{extract_descriptives_mbp_jags}
\alias{extract_descriptives_mbp_jags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_descriptives_mbp_jags(jags.object.base, delta, prec.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jags.object.base}{
%%     ~~Describe \code{jags.object.base} here~~
}
  \item{delta}{
%%     ~~Describe \code{delta} here~~
}
  \item{prec.name}{
%%     ~~Describe \code{prec.name} here~~
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
function (jags.object.base, delta, prec.name) 
{
    descriptive_matrix_minus <- extract_descriptives_jags(jags.object.base, 
        w = 1 - delta, prec.name)
    descriptive_matrix_base <- extract_descriptives_jags(jags.object.base, 
        w = 1, prec.name)
    descriptive_matrix_plus <- extract_descriptives_jags(jags.object.base, 
        w = 1 + delta, prec.name)
    descriptive.matrix <- cbind(cbind(descriptive_matrix_minus, 
        descriptive_matrix_base), descriptive_matrix_plus)
    colnames(descriptive.matrix) <- c("mean.minus", "sd.minus", 
        "mean.base", "sd.base", "mean.plus", "sd.plus")
    return(list(descriptive.matrix = descriptive.matrix, delta = delta))
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

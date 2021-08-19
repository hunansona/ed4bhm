\name{ed}
\alias{ed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ed(descriptive.matrix.ls, distance = "H2")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{descriptive.matrix.ls}{
%%     ~~Describe \code{descriptive.matrix.ls} here~~
}
  \item{distance}{
%%     ~~Describe \code{distance} here~~
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
function (descriptive.matrix.ls, distance = "H2") 
{
    if (distance == "H2") {
        D_func <- D_H2
    }
    else if (distance == "BCsd") {
        D_func <- D_BCsd
    }
    else if (distance == "BCmu") {
        D_func <- D_BCmu
    }
    else {
        cat("The distance must be one of the following: H2, BCsd, BCmu.")
    }
    return(apply(descriptive.matrix.ls[["descriptive.matrix"]], 
        1, d2_interface_vector, delta = descriptive.matrix.ls[["delta"]], 
        method = distance))
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

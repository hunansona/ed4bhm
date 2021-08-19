\name{ed.jags}
\alias{ed.jags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ed.jags(jags.object.base, delta = 0.01, prec.name, distance = "H2")
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
function (jags.object.base, delta = 0.01, prec.name, distance = "H2") 
{
    descriptive.matrix.ls <- extract_descriptives_mbp_jags(jags.object.base, 
        delta, prec.name)
    if (distance == "H2ALL") {
        ed.result <- H2ALL(descriptive.matrix.ls)
    }
    else {
        ed.result <- ed(descriptive.matrix.ls, distance)
    }
    rownames(ed.result) <- rownames(descriptive.matrix.ls$descriptive.matrix)
    return(list(ed = ed.result, descriptive.matrix.ls = descriptive.matrix.ls))
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

\name{d2_interface_vector}
\alias{d2_interface_vector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
d2_interface_vector(avector, delta, method)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{avector}{
%%     ~~Describe \code{avector} here~~
}
  \item{delta}{
%%     ~~Describe \code{delta} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
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
function (avector, delta, method) 
{
    if (method == "H2") {
        D_func <- D_H2
    }
    else if (method == "BCmu") {
        D_func <- D_BCmu
    }
    else if (method == "BCsd") {
        D_func <- D_BCsd
    }
    else {
        cat("The method must be one of the following: H2, BCmu, BCsd.")
    }
    if (method \%in\% c("BCsd")) {
        return((D_func(sdb = avector[4], sda = avector[6]) - 
            2 * D_func(sdb = avector[4], sda = avector[4]) + 
            D_func(sdb = avector[4], sda = avector[2]))/delta^2)
    }
    else {
        return((D_func(mb = avector[3], sdb = avector[4], ma = avector[5], 
            sda = avector[6]) - 2 * D_func(mb = avector[3], sdb = avector[4], 
            ma = avector[3], sda = avector[4]) + D_func(mb = avector[3], 
            sdb = avector[4], ma = avector[1], sda = avector[2]))/delta^2)
    }
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

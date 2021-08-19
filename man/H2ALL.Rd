\name{H2ALL}
\alias{H2ALL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
H2ALL(descriptive.matrix.ls)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{descriptive.matrix.ls}{
%%     ~~Describe \code{descriptive.matrix.ls} here~~
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
function (descriptive.matrix.ls) 
{
    H2ALL_matrix <- matrix(NA, nrow = nrow(descriptive.matrix.ls[["descriptive.matrix"]]), 
        ncol = 5)
    colnames(H2ALL_matrix) <- c("H2total_d2", "H2mu_d2", "H2sd_d2", 
        "qH2mu_total_d2", "qH2sd_total_d2")
    H2ALL_matrix[, "H2total_d2"] <- ed(descriptive.matrix.ls = descriptive.matrix.ls, 
        distance = "H2")
    H2ALL_matrix[, "H2mu_d2"] <- -ed(descriptive.matrix.ls = descriptive.matrix.ls, 
        distance = "BCmu")
    H2ALL_matrix[, "H2sd_d2"] <- -ed(descriptive.matrix.ls = descriptive.matrix.ls, 
        distance = "BCsd")
    H2ALL_matrix[, "qH2mu_total_d2"] <- -ed(descriptive.matrix.ls = descriptive.matrix.ls, 
        distance = "BCmu")/ed(descriptive.matrix.ls = descriptive.matrix.ls, 
        distance = "H2")
    H2ALL_matrix[, "qH2sd_total_d2"] <- -ed(descriptive.matrix.ls = descriptive.matrix.ls, 
        distance = "BCsd")/ed(descriptive.matrix.ls = descriptive.matrix.ls, 
        distance = "H2")
    return(H2ALL_matrix)
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

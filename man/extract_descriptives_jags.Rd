\name{extract_descriptives_jags}
\alias{extract_descriptives_jags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_descriptives_jags(jags.object.base, w, prec.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jags.object.base}{
%%     ~~Describe \code{jags.object.base} here~~
}
  \item{w}{
%%     ~~Describe \code{w} here~~
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
function (jags.object.base, w, prec.name) 
{
    descriptive.matrix <- NULL
    deviance_index <- which(colnames(jags.object.base[[1]]) == 
        "deviance")
    pars_i_sample <- as.matrix(jags.object.base)
    if (!is.null(prec.name)) {
        pars_i_sample[, prec.name] <- log(pars_i_sample[, prec.name])
    }
    pars_i_sample <- pars_i_sample[, -deviance_index]
    probs_i_sample <- weightFC_jags(jags.object.base, w)
    samples.matrix <- as.matrix(pars_i_sample, nrow = length(pars_i_sample))
    means <- apply(samples.matrix, 2, expectation_fc, ISweight = probs_i_sample)
    sds <- apply(samples.matrix, 2, sd_fc, ISweight = probs_i_sample)
    descriptive.matrix <- cbind(means, sds)
    row.names(descriptive.matrix) <- colnames(jags.object.base[[1]])[-deviance_index]
    colnames(descriptive.matrix) <- c("mean", "sd")
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

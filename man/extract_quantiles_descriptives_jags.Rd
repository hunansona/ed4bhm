\name{extract_quantiles_descriptives_jags}
\alias{extract_quantiles_descriptives_jags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_quantiles_descriptives_jags(jags.object.base, w, prec.name)
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
    job <- jags.object.base
    pars_i_sample <- as.matrix(job)
    if (!is.null(prec.name)) {
        pars_i_sample[, prec.name] <- log(pars_i_sample[, prec.name])
    }
    pars_i_sample <- pars_i_sample[, -deviance_index]
    probs_i_sample <- weightFC_jags(jags.object.base, w)
    samples.matrix <- as.matrix(pars_i_sample, nrow = length(pars_i_sample))
    quant0.025 <- apply(samples.matrix, 2, quantile, probs = 0.025)
    quant0.5 <- apply(samples.matrix, 2, quantile, probs = 0.5)
    quant0.975 <- apply(samples.matrix, 2, quantile, probs = 0.975)
    descriptive.matrix <- cbind(quant0.025, quant0.5, quant0.975)
    row.names(descriptive.matrix) <- colnames(jags.object.base[[1]])[-deviance_index]
    colnames(descriptive.matrix) <- c("0.025quant", "0.5quant", 
        "0.975quant")
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

\name{weightFC_jags}
\alias{weightFC_jags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Probability weights in JAGS
%%  ~~function to do ... ~~
}
\description{Computes probability weights by fast computation in JAGS.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weightFC_jags(jags.obj, w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jags.obj}{character string, the name of the JAGS object fitted with \code{rjags} with the base model (non-weighted likelihood)
%%     ~~Describe \code{jags.obj} here~~
}
  \item{w}{numeric, weighting factor to weight the likelihood. \code{w} attains values close to 1.
%%     ~~Describe \code{w} here~~
}
}
\details{A fast computation formula is derived (Hunanyan et al., 2021) to estimate the weighted samples for the model fitted in JAGS.
%%  ~~ If necessary, more details than the description above ~~
}
\value{numeric vector
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Hunanyan, S., Roos, M., Plummer, M., Rue, H. (2021). Quantification of empirical determinacy: the impact of likelihood weighting on posterior location and spread in Bayesian meta-analysis estimated with JAGS and INLA. \emph{Bayesian Analaysis} (in preparation)
%% ~put references to the literature/web site here ~
}
\seealso{\code{\link{expectation_fc}}, \code{\link{sd_fc}}, \code{\link{extract_descriptives_jags}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
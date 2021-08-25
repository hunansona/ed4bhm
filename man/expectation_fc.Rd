\name{expectation_fc}
\alias{expectation_fc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Expectation of an MCMC weighted sample
%%  ~~function to do ... ~~
}
\description{Computes the expectation of an MCMC weighted sample via impotance sampling.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
expectation_fc(ISweight, samples)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ISweight}{numeric vector, the importance sampling weights
%%     ~~Describe \code{ISweight} here~~
}
  \item{samples}{the MCMC samples to calculate the expectation for, extracted from the JAGS fit
%%     ~~Describe \code{samples} here~~
}
}
\details{A fast computation formula is derived (Hunanyan et al., 2021) to estimate the weighted samples for the model fitted in JAGS.
%%  ~~ If necessary, more details than the description above ~~
}
\value{numeric, expectation of the weighted MCMC sample
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Hunanyan, S., Roos, M., Plummer, M., Rue, H. (2021). Quantification of empirical determinacy: the impact of likelihood weighting on posterior location and spread in Bayesian meta-analysis estimated with JAGS and INLA. \emph{Bayesian Analaysis} (in preparation)
%% ~put references to the literature/web site here ~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{sd_fc}}, \code{\link{extract_descriptives_jags}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
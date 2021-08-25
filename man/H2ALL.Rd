\name{H2ALL}
\alias{H2ALL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Empirical determinacy estimates for all H2 types of measures
%%  ~~function to do ... ~~
}
\description{This function calculates a matrix with columns representing empirical determiancy measure estimates for all squared Hellinger (H2) distance types of five measures, which are TED, EDL, EDS, pEDL and pEDS defined in Hunanyat et al., 2021.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
H2ALL(descriptive.matrix.ls)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{descriptive.matrix.ls}{is a list composed of the descriptive matrix and delta
%%     ~~Describe \code{descriptive.matrix.ls} here~~
}
}
\details{The descriptive matrix has six columns containing means and standard deviations for minus, base and plus model fits and \eqn{\delta} is the numerical step with \eqn{w = 1-\delta} and \eqn{w = 1 + \delta} being the factor to weight the likelihood. \code{descriptive.matrix.ls} must be an output of \code{\link{extract_descriptives_mbp_inla}} or \code{\link{extract_descriptives_mbp_jags}}.

The columns of the returned matrix are colled "H2total_d2", "H2mu_d2", "H2sd_d2", "qH2mu_total_d2", "qH2sd_total_d2" for the measures TED, EDL, EDS, pEDL and pEDS, respectively. The rows of this matrix correspond to model parameters.
%%  ~~ If necessary, more details than the description above ~~
}
\value{numeric matrix
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

\seealso{\code{\link{extract_descriptives_mbp_inla}}, \code{\link{extract_descriptives_mbp_jags}}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\name{Lwm1_stan}
\alias{Lwm1_stan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighting the log-likelihood from Stan
}
\description{Computes probability weights by fast computation (Hunanyan et al., 2021) in Stan.
}
\usage{
Lwm1_stan(stan_ll, ww = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stan_ll}{the sample of the log-likelihood calculated from a stan fit}
  \item{ww}{the weighting factor}
}
\author{
Malgorzata Roos, Georgios Kazantzidis, Sona Hunanyan
}
\references{Hunanyan, S., Roos, M., Plummer, M., Rue, H. (2021). Quantification of empirical determinacy: the impact of likelihood weighting on posterior location and spread in Bayesian meta-analysis estimated with JAGS and INLA. \emph{Bayesian Analaysis} (under review). https://arxiv.org/abs/2109.11870.
%% ~put references to the literature/web site here ~
}
\name{D_BCmu}
\alias{D_BCmu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The location part of the Bhattacharyya coefficient (BC) between two normal densities
%%  ~~function to do ... ~~
}
\description{Computes the Bhattacharyya coefficient (the location-part) between two normal densities with means \code{mb} and \code{ma} and standard deviations \code{sdb} and \code{sda}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
D_BCmu(mb, sdb, ma, sda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mb}{mean of the first (base) normal density
%%     ~~Describe \code{mb} here~~
}
  \item{sdb}{standard deviation of the first (base) normal density
%%     ~~Describe \code{sdb} here~~
}
  \item{ma}{mean of the second (weighted) normal density
%%     ~~Describe \code{ma} here~~
}
  \item{sda}{standard deviation of the second (weighted) normal density
%%     ~~Describe \code{sda} here~~
}
}
\details{The function returns the location-part of the BC under normal approximation according to Roos et al. (2021). The location-part is adjusted for standard deviations which corresponds to the Mahalanobis distance.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A numeric value
}
\references{Roos, M., Hunanyan, S., Bakka, H., Rue, H. (2020). Sensitivity and identification quantification by a relative latent model complexity perturbation in the Bayesian meta-analysis. \emph{Biometrical Journal}.
%% ~put references to the literature/web site here ~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{D_H2}}, \code{\link{D_BCsd}}
}
\examples{
D_BCmu(mb=0.4, sdb=0.3, ma=0.42, sda=0.32)
}

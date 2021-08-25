\name{D_BCsd}
\alias{D_BCsd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The spread part of the Bhattacharyya coefficient (BC) between two normal densities
%%  ~~function to do ... ~~
}
\description{Computes the Bhattacharyya coefficient (the spread-part) between two normal densities with standard deviations \code{sdb} and \code{sda}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
D_BCsd(sdb, sda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sdb}{standard deviation of the first (base) normal density
%%     ~~Describe \code{sdb} here~~
}
  \item{sda}{standard deviation of the second (weighted) normal density
%%     ~~Describe \code{sda} here~~
}
}
\details{The function returns the spread-part of the BC under normal approximation according to Roos et al. (2021). This part quantifies the spread modification.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A numeric value
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Roos, M., Hunanyan, S., Bakka, H., Rue, H. (2020). Sensitivity and identification quantification by a relative latent model complexity perturbation in the Bayesian meta-analysis. \emph{Biometrical Journal}.
%% ~put references to the literature/web site here ~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{D_H2}}, \code{\link{D_BCmu}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
D_BCsd(sdb=0.3, sda=0.32)
}

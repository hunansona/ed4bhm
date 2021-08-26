\name{eight_schools}
\alias{eight_schools}
\docType{data}
\title{Eight schools data
}
\description{Estimated effect of a coaching program on SAT-V scores y and its standard error
(sigma) in eight schools.
}
\usage{data(eight_schools)}
\format{
  A data frame with 8 observations on the following 5 variables.
  \describe{
    \item{\code{school}}{character string, labels of the schools}
    \item{\code{y}}{numeric, the estimated coaching effects on eight schools}
    \item{\code{sigma}}{numeric, standard error of the coaching effect of each school}
    \item{\code{prec}}{numeric, precision of the coaching effect of each school}
    \item{\code{schooln}}{integer, labels of the schools}
  }
}
\details{To quantify the effect of a coaching program on SAT-V (Scholastic Aptitude Test-Verbal)
scores in eight high schools, data from a randomized study were pre-analyzed and
supplied for a Bayesian meta-analysis.
}
\source{A. Gelman, J.B. Carlin, H.S. Stern, D.B. Dunson, A. Vehtari, and D.B. Rubin. Bayesian
Data Analysis. 3rd Edition. \emph{Chapman and Hall/CRC Press}, Boca Raton, 2014.
}
\references{A. Gelman and J. Hill. Data Analysis Using Regression and Multilevel/Hierarchical Models.
\emph{Cambridge University Press}, Cambridge, 2007.

A. Vehtari, A. Gelman, D. Simpson, B. Carpenter, and P. Burkner. Rank-normalization, 
folding, and localization: An improved R_hat for assessing convergence of MCMC (with
Discussion). \emph{Bayesian Analysis}, \bold{16}(2):667718, 2021.
}
\examples{
data(eight_schools)
str(eight_schools)
}
\keyword{datasets}

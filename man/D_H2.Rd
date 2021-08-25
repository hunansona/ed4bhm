\name{D_H2}
\alias{D_H2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bhattacharyya coefficient between two normal densities
%%  ~~function to do ... ~~
}
\description{Computes the squared Hellinger distance between two normal densities parametrized by means \code{mb}, \code{ma} and standard deviations \code{sdb}, \code{sda}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
D_H2(mb, sdb, ma, sda)
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
\details{BC is a symmetric measure of affinity (Roos et al., 2015). 
\deqn{BC(\pi_1(\psi | y), \pi_w(\psi | y)) = \int_{-\infty}^{\infty} \sqrt{\pi_1(\psi | y) \pi_w(\psi | y)} d \psi}.

\deqn{H^2} between two normal densities is 
\deqn{BC(\pi^N_1, \pi^N_w) = \sqrt{\frac{\sigma_1 \sigma_w}{\frac{\sigma^2_1+\sigma^2_w}{2} }} \exp ( -\frac{ (\mu_w-\mu_1)^2 }{4(\sigma^2_w + \sigma^2_1)} ),}
%%  ~~ If necessary, more details than the description above ~~
}
\value{A numeric value.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Roos, M., Martins, T., Held, L., Rue, H. (2015). Sensitivity analysis for Bayesian hierarchical models. \emph{Bayesian Analysis} \bold{10}(2), 321-349. https://projecteuclid.org/euclid.ba/1422884977
%% ~put references to the literature/web site here ~
}
\seealso{\code{\link{D_BCsd}}, \code{\link{D_BCmu}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
D_H2(mb=0.4, sdb=0.3, ma=0.42, sda=0.32)
}
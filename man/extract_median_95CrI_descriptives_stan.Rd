\name{extract_median_95CrI_descriptives_stan}
\alias{extract_median_95CrI_descriptives_stan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of the median and the equi-tailed 95\% CrI from a Stan object
%%  ~~function to do ... ~~
}
\description{This function takes a Stan object as input and extracts the median and the equi-tailed 95\% CrI of all the model parameters (without random effects).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract_median_95CrI_descriptives_stan(stan.object.base.modified, 
                                                   w, prec.name= NULL)
}
\arguments{
  \item{stan.object.base.modified}{character string, the name of the Stan object fitted in \code{rstan} 
}
  \item{w}{numeric, the weighting factor to weight the likelihood. For the base model \eqn{w=1}.
}
  \item{prec.name}{character string, the name of the precision parameter to be used for logarithmic transformation of the marginal posterior samples of the precision parameter.
}
}
\details{The returned matrix has three columns with names "0.025quant", "0.5quant" and "0.975quant" for the median and the equi-tailed 95\% CrI of the marginal posterior distributions and has as many rows as there are parameters (without random effects) in the model.
}
\value{numerical matix
}
\seealso{\code{\link{rstan}}
}

\name{ed.stan.input}
\alias{ed.stan.input}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Input for ed.stan
}
\description{
Converts a stanfit to a matrix. The matrix includes all the samples and the column called ll in the correct order
}
\usage{
ed.stan.input(object_t)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object_t}{stanfit that will be transformed to matrix.}
}
\value{
The matrix version of a stanfit object. 
}

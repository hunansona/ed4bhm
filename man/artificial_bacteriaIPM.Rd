\name{artificial_bacteriaIPM}
\alias{artificial_bacteriaIPM}
\docType{data}
\title{Artificial dataset generated from the bacterial resistance data
}
\description{Artificial dataset generated from the bacterial resistance data corresponding to the data for antibiotic Imipenem (IPM).
}
\usage{data(artificial_bacteriaIPM)}
\format{
  A data frame with 38 observations on the following 23 variables.
  \describe{
    \item{\code{IZD}}{integer, Inhibition zone diameter (IZD) values in (mm), which can attain values between 3 (mm) and 40 (mm)}
        \item{\code{scIZD}}{numeric, to have mean 0 and standard deviation 1, which can attain values between -1.66 and 1.66}
    \item{\code{nipmwt}}{numeric, counts of the wild-type isolates for the antibiotic IPM }
    \item{\code{nipmesbl}}{numeric, original counts of the ESBL isolates for the antibiotic IPM}
    \item{\code{nipmesb1, nipmesb2, nipmesb3, nipmesb4, nipmesb5, nipmesb6, nipmesb7, nipmesb8, nipmesb9, nipmesb10, nipmesb11, nipmesb12, nipmesb13, nipmesb14, nipmesb15, nipmesb16, nipmesb17, nipmesb18, nipmesbl19}}{numeric, artificial counts of the ESBL isolates for the antibiotic IPM shifted towards lower values of IZD}
  }
}
\details{We have generates artificial dataset given the data corresponding to antibiotic IPM in \code{\link{bacterial_resistance}}. Artificially generated columns \code{nipmesbl1} \eqn{\ldots} \code{nipmesbl19} ensure different separation levels in the data.
}
\seealso{\code{\link{bacterial_resistance}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\source{Hunanyan, S. and Kazantzidis, G. and Rue, H. and Roos, M (2021). Empirical determinacy in the {B}ayesian logistic regression in the presence of separation in the data (in preparation).}

\references{G. Valsesia, M. Hombach, F.P. Maurer, P. Courvalin, M. Roos, and E.C. Boettger. The
resistant-population cutoff (RCOFF): a new concept for improved characterization of antimicrobial
susceptibility patterns of non-wild-type bacterial populations.
\emph{Journal of Clinical Microbiology}, 53(6):1806-1811, 2015a.

G. Valsesia, M. Roos, E.C. Boettger, and M. Hombach. A statistical approach for determination
of disk diffusion-based cutoff values for systematic characterization of wild-type
and non-wild-type bacterial populations in antimicrobial susceptibility testing. \emph{Journal of
Clinical Microbiology}, 53(6):1812-1822, 2015b.
}
\examples{
data(artificial_bacteriaIPM)
str(artificial_bacteriaIPM)
}
\keyword{datasets}

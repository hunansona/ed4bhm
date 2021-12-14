\name{bacterial_resistance}
\alias{bacterial_resistance}
\docType{data}
\title{Bacterial resistance data
}
\description{Strains of \emph{E. coli}, which were isolated from 2010 until 2014 in the laboratory of the Institute for Medical Microbiology at the University of Zurich, are considered. Inhibition zone diameter (IZD) values in (mm) for the antibiotics Amoxicillin-clavulanic acid (AMC), Ceftriaxone (CRO), and Imipenem (IPM). IZD can attain values between 6 (mm) and 40 (mm).
}
\usage{data(bacterial_resistance)}
\format{
  A data frame with 35 observations on the following 8 variables.
  \describe{
    \item{\code{IZD}}{numeric, inhibition zone diameter (IZD) values in (mm). IZD can attain values between 6 (mm) and 40 (mm).}
    \item{\code{scIZD}}{numeric, inhibition zone diameter (IZD) values scaled to have mean 0 and standard deviation 1.}
    \item{\code{ncrowt}}{numeric, counts for the wilde-type isolates for the antibiotic CRO.}
    \item{\code{ncroesbl}}{numeric, counts for the ESBL isolates for the antibiotic CRO.}
    \item{\code{nipmwt}}{numeric, counts for the wilde-type isolates for the antibiotic IPM.}
    \item{\code{nipmesbl}}{numeric, counts for the ESBL isolates for the antibiotic IPM.}
    \item{\code{namcwt}}{numeric, counts for the wilde-type isolates for the antibiotic AMC.}
    \item{\code{namcesbl}}{numeric, counts for the ESBL isolates for the antibiotic AMC.}
  }
}
\details{These isolates were identified as resistant (ESBL) and susceptible (wild-type) bacteria through antimicrobial susceptibility testing (AST).
For AST, bacterial isolates are brushed on agar plates.
An antibiotic is provided on a disk in the middle of the agar plate.
After eight hours of incubation, the inhibition zone diameter (IZD) is quantified.
}
\source{G. Valsesia, M. Hombach, F.P. Maurer, P. Courvalin, M. Roos, and E.C. Boettger. The
resistant-population cutoff (RCOFF): a new concept for improved characterization of antimicrobial
susceptibility patterns of non-wild-type bacterial populations.
\emph{Journal of Clinical Microbiology}, 53(6):1806-1811, 2015a.}

\references{G. Valsesia, M. Roos, E.C. Boettger, and M. Hombach. A statistical approach for determination
of disk diffusion-based cutoff values for systematic characterization of wild-type
and non-wild-type bacterial populations in antimicrobial susceptibility testing. \emph{Journal of
Clinical Microbiology}, 53(6):1812-1822, 2015b.
}
\examples{
data(bacterial_resistance)
str(bacterial_resistance)
}
\keyword{datasets}

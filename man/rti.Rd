\name{rti}
\alias{rti}
\docType{data}
\title{Respiratory tract infections data
}
\description{
Meta-analysis data set including 22 randomized, controlled clinical trials on the prevention of respiratory tract infections by selective decontamination of the digestive tract in intensive care unit patients. The patients in the treated group received oral antibiotics, and those in the control groups received no prophylaxis.}
\usage{data(rti)}
\format{
  A data frame with 22 observations on the following 7 variables.
  \describe{
    \item{\code{study}}{character string, label of the study}
    \item{\code{f.t}}{integer, number of patients who became cases of respiratory tract infections among those receiving antibiotics}
    \item{\code{n.t}}{integer, number of patients receiving antibiotics}
    \item{\code{f.c}}{integer, number of patients who became cases of respiratory tract infections among those receiving no prophylaxis}
    \item{\code{n.c}}{integer, number of patients receiving no prophylaxis (control group)}
    \item{\code{y}}{numeric, log odds ratio estimates for the individual trials}
    \item{\code{sigma}}{numeric, standard error of the log odds ratio estimate for the individual trials}
  }
}
\details{The data set was originally analyzed by the Selective Decontamination of the Digestive Tract Trialists' Collaborative Group (1993) and reanalyzed by Bodnar et al. (2017). Overall, 3836 patients are included in these 22 trials.

The log odds ratio estimates and standard errors were computed using the escalc function in the package metafor using the option of adding 0.5 to all cells in tables including zeros (argument to="only0") as described in Bodnar et al. (2017).
}
\source{Bodnar, O., Link, A., Arendacka, B., Possolo, A., Elster, C. (2017). Bayesian estimation in random effects meta-analysis using a non-informative prior. \emph{Statistics in Medicine} \bold{36}, 378–399.
}
\references{Selective Decontamination of the Digestive Tract Trialists' Collaborative Group (1993). Meta-analysis of randomised controlled trials of selective decontamination of the digestive tract. \emph{British Medical Journal} \bold{307(6903)}, 525–532.
}
\examples{
data(rti)
str(rti)
# forest plot
forest(x=rti$y, sei=rti$sigma, xlab="log odds ratio")
}
\keyword{datasets}

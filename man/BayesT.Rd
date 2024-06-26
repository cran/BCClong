% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DiscrepancyMeasure.R
\name{BayesT}
\alias{BayesT}
\title{Goodness of fit.}
\usage{
BayesT(fit)
}
\arguments{
\item{fit}{an objective output from BCC.multi() function}
}
\value{
Returns a dataframe with length equals to 2 that contains
observed and predict value
}
\description{
This function assess the model goodness of fit by calculate the
discrepancy measure T(bm(y), bm(Theta)) with following steps
(a) Generate T.obs based on the MCMC samples
(b) Generate T.rep based on the posterior distribution of the parameters
(c) Compare  T.obs and T.rep, and calculate the P values.
}
\examples{
#import data
data(example)
fit.BCC <- example
BayesT(fit.BCC)

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelSelection.R
\name{model.selection.criteria}
\alias{model.selection.criteria}
\title{Model selection}
\usage{
model.selection.criteria(fit, fast_version = TRUE)
}
\arguments{
\item{fit}{an objective output from BCC.multi() function}

\item{fast_version}{if fast_verion=TRUE (default), then compute the DIC and WAIC using
the first 100 MCMC samples (after burn-in and thinning) . If fast_version=FALSE, then
compute the DIC and WAIC using all MCMC samples (after burn-in and thinning)}
}
\value{
Returns the calculated score
}
\description{
A function that calculates DIC and WAIC for model selection
}
\examples{
#import data
data(example1)
fit.BCC <- example1
res <- model.selection.criteria(fit.BCC, fast_version=TRUE)
res

}

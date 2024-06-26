% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bccLong.R
\name{BCC.multi}
\alias{BCC.multi}
\title{Compute a Bayesian Consensus Clustering model for mixed-type longitudinal data}
\usage{
BCC.multi(
  mydat,
  id,
  time,
  center = 1,
  num.cluster,
  formula,
  dist,
  alpha.common = 0,
  initials = NULL,
  sigma.sq.e.common = 1,
  hyper.par = list(delta = 1, a.star = 1, b.star = 1, aa0 = 0.001, bb0 = 0.001, cc0 =
    0.001, ww0 = 0, vv0 = 1000, dd0 = 0.001, rr0 = 4, RR0 = 3),
  c.ga.tunning = NULL,
  c.theta.tunning = NULL,
  adaptive.tunning = 0,
  tunning.freq = 20,
  initial.cluster.membership = "random",
  input.initial.local.cluster.membership = NULL,
  input.initial.global.cluster.membership = NULL,
  seed.initial = 2080,
  burn.in,
  thin,
  per,
  max.iter
)
}
\arguments{
\item{mydat}{list of R longitudinal features (i.e., with a length of R),
where R is the number of features. The data should be prepared
in a long-format (each row is one time point per individual).}

\item{id}{a list (with a length of R) of vectors of the study id of
individuals for each feature. Single value (i.e., a length of 1)
is recycled if necessary}

\item{time}{a list (with a length of R) of vectors of time (or age) at which
the feature measurements are recorded}

\item{center}{1: center the time variable before clustering, 0: no centering}

\item{num.cluster}{number of clusters K}

\item{formula}{a list (with a length of R) of formula for each feature.
Each formula is a twosided linear formula object describing
both the fixed-effects and random effects part of the model,
with the response (i.e., longitudinal feature) on the left
of a ~ operator and the terms, separated by + operations,
or the right. Random-effects terms are distinguished by
vertical bars (|) separating expressions for design matrices
from grouping factors.
See formula argument from the lme4 package}

\item{dist}{a character vector (with a length of R) that determines the
distribution for each feature. Possible values are "gaussian"
for a continuous feature, "poisson" for a discrete feature
(e.g., count data) using a log link and "binomial" for a
dichotomous feature (0/1) using a logit link. Single value
(i.e., a length of 1) is recycled if necessary}

\item{alpha.common}{1 - common alpha, 0 - separate alphas for each outcome}

\item{initials}{List of initials for: zz, zz.local ga, sigma.sq.u, sigma.sq.e,
Default is NULL}

\item{sigma.sq.e.common}{1 - estimate common residual variance across all groups,
0 - estimate distinct residual variance, default is 1}

\item{hyper.par}{hyper-parameters of the prior distributions for the model
parameters. The default hyper-parameters values will result
in weakly informative prior distributions.}

\item{c.ga.tunning}{tuning parameter for MH algorithm (fixed effect parameters),
each parameter corresponds to an outcome/marker, default
value equals NULL}

\item{c.theta.tunning}{tuning parameter for MH algorithm (random effect),
each parameter corresponds to an outcome/marker,
default value equals NULL}

\item{adaptive.tunning}{adaptive tuning parameters, 1 - yes, 0 - no,
default is 1}

\item{tunning.freq}{tuning frequency, default is 20}

\item{initial.cluster.membership}{"mixAK" or "random" or "PAM" or "input" -
input initial cluster membership for local
clustering, default is "random"}

\item{input.initial.local.cluster.membership}{if use "input",
option input.initial.cluster.membership
must not be empty, default is NULL}

\item{input.initial.global.cluster.membership}{input initial cluster
membership for global clustering
default is NULL}

\item{seed.initial}{seed for initial clustering
(for initial.cluster.membership = "mixAK")
default is 2080}

\item{burn.in}{the number of samples disgarded.
This value must be smaller than max.iter.}

\item{thin}{the number of thinning. For example, if thin = 10,
then the MCMC chain will keep one sample every 10 iterations}

\item{per}{specify how often the MCMC chain will print the iteration number}

\item{max.iter}{the number of MCMC iterations.}
}
\value{
Returns a BCC class model contains clustering information
}
\description{
This function performs clustering on mixed-type (continuous, discrete and
categorical) longitudinal markers using Bayesian consensus clustering method
with MCMC sampling
}
\examples{
# import dataframe
data(epil)
# example only, larger number of iteration required for accurate result
fit.BCC <-  BCC.multi (
       mydat = list(epil$anxiety_scale,epil$depress_scale),
       dist = c("gaussian"),
       id = list(epil$id),
       time = list(epil$time),
       formula =list(y ~ time + (1|id)),
       num.cluster = 2,
       burn.in = 3,
       thin = 1,
       per =1,
       max.iter = 8)

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biglm_vcov.R
\name{update_sandwich}
\alias{update_sandwich}
\title{Update HC sandwich estimator}
\usage{
update_sandwich(qr, mm, n, p, y, offset, w)
}
\arguments{
\item{qr}{BoundedQr object}

\item{mm}{}

\item{n}{number of observations}

\item{p}{number of covariates}

\item{y}{response}

\item{offset}{response offset}

\item{w}{weights}
}
\description{
Update HC sandwich estimator
}
\keyword{internal}

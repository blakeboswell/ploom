\name{Rcpp_BoundedQr-class}
\Rdversion{1.1}
\docType{class}
\alias{Rcpp_BoundedQr-class}

\title{Class \code{"Rcpp_BoundedQr"}}
\description{
  Object to support out of memory QR factorizations.
}
\section{Extends}{
Class \code{"\linkS4class{C++Object}"}, directly.

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}

\examples{
showClass("Rcpp_BoundedQr")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{D}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{num_obs}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{num_params}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{pweights}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{rbar}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{rss_full}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{sumsqy}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{thetab}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{tol}:}{Object of class \code{activeBindingFunction} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{initialize(...)}:}{ ~~ }
    \item{\code{finalize()}:}{ ~~ }
    \item{\code{vcov(...)}:}{ ~~ }
    \item{\code{update(...)}:}{ ~~ }
    \item{\code{rss()}:}{ ~~ }
    \item{\code{rank()}:}{ ~~ }
    \item{\code{lindep()}:}{ ~~ }
    \item{\code{beta()}:}{ ~~ }
  }
}

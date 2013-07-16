\name{pva-class}
\Rdversion{1.1}
\docType{class}
\alias{pva-class}
\title{Class \code{"pva"}}
\description{
Model class for fitted PVA objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pva", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{observations}:}{Object of class \code{"numeric"},
        vector of observations (must be non-negative but not necessarily
        integer), possibly with missing values (\code{NA}). }
    \item{\code{model}:}{Object of class \code{"pvamodel"},
        internal representation of the growth model and observation error
        structure.}
    \item{\code{summary}:}{Object of class \code{"matrix"},
        asymptotic Wald-type summary on the 'original'
        scale of the parameters (i.e. not on the scale used
        for model fitting and diagnostics).}
    \item{\code{dcdata}:}{Object of class \code{"dcFit"},
        internal representation of the data and JAGS model.}
    \item{\code{call}:}{Object of class \code{"language"},
        the call.}
    \item{\code{coef}:}{Object of class \code{"numeric"},
        point estimates of the model parameters.}
    \item{\code{fullcoef}:}{Object of class \code{"numeric"},
        vector possibly containing fixed parameter values.}
    \item{\code{vcov}:}{Object of class \code{"matrix"},
        variance covariance matrix of the estimates.}
    \item{\code{details}:}{Object of class \code{"dcCodaMCMC"},
        MCMC output from data cloning.}
    \item{\code{nobs}:}{Object of class \code{"integer"},
        number of observations (excluding missing values).}
    \item{\code{method}:}{Object of class \code{"character"},
        optimization method (data cloning).}
  }
}
\section{Extends}{
Class \code{"\linkS4class{dcmle}"}, directly.
}
\section{Methods}{
  \describe{
    \item{as.mcmc.list}{\code{signature(x = "pva")}}
    \item{coef}{\code{signature(object = "pva")}}
    \item{confint}{\code{signature(object = "pva")}}
    \item{show}{\code{signature(object = "pva")}}
    \item{vcov}{\code{signature(object = "pva")}}
  }
}
\author{
Khurram Nadeem and Peter Solymos
}
\seealso{
\code{\link{pva}}
}
\examples{
showClass("pva")
}
\keyword{classes}

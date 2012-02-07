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
      population abundances.}
    \item{\code{nobs}:}{Object of class \code{"integer"}, 
      number of observations (not effective sample size!).}
    \item{\code{model}:}{Object of class \code{"pvamodel"},
      growth and observation error model info.}
    \item{\code{dcdata}:}{Object of class \code{"dcFit"},
      data used in model fitting.}
    \item{\code{vcov}:}{Object of class \code{"matrix"},
      variance covariance matrix (true parameter scale).}
    \item{\code{mcmc}:}{Object of class \code{"MCMClist"},
      fitted mcmc.list (diagnostics scale).}
    \item{\code{summary}:}{Object of class \code{"matrix"},
      anymptotic inference summary (true parameter scale).}
    \item{\code{dctable}:}{Object of class \code{"dcTable"},
      data cloning summaries (diagnostics scale).}
    \item{\code{dcdiag}:}{Object of class \code{"dcDiag"},
      data cloning diagnostics (diagnostics scale).}
    \item{\code{start}:}{Object of class \code{"numeric"},
      start of MCMC samples.}
    \item{\code{end}:}{Object of class \code{"numeric"},
      end of MCMC samples.}
    \item{\code{thin}:}{Object of class \code{"numeric"},
      thinning value.}
    \item{\code{n.chains}:}{Object of class \code{"numeric"},
      number of chains.}
    \item{\code{n.clones}:}{Object of class \code{"nClones"},
      number of clones.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{dcMle}"}, directly.
}
\section{Methods}{
  \describe{
    \item{as.mcmc.list}{\code{signature(x = "pva")}: coercion method.}
    \item{coef}{\code{signature(object = "pva")}: coefficients
      (including fixed values)}
    \item{confint}{\code{signature(object = "pva")}: 
      asyptotic Wald-type confidence intervals.}
    \item{show}{\code{signature(object = "pva")}: 
      fancy output.}
    \item{vcov}{\code{signature(object = "pva")}: 
      asymptotic variance covariance matrix.}
	 }
}
\author{
Khurram NAdeem and Peter Solymos
}
\seealso{
\code{\link{pva}}
}
\examples{
showClass("pva")
}
\keyword{classes}

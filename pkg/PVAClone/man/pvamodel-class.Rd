\name{pvamodel-class}
\Rdversion{1.1}
\docType{class}
\alias{pvamodel-class}

\title{Class \code{"pvamodel"}}
\description{
S4 class for predefined PVA models.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pvamodel", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{growth.model}:}{Object of class \code{"character"},
      name of growth model.}
    \item{\code{obs.error}:}{Object of class \code{"character"},
      name of observation error type (\code{"none"}, 
      \code{"poisson"}, \code{"normal"}).}
    \item{\code{model}:}{Object of class \code{"dcModel"},
      BUGS model for estimation.}
    \item{\code{predmodel}:}{Object of class \code{"dcModel"},
      BUGS model for prediction.}
    \item{\code{p}:}{Object of class \code{"integer"},
      number of parameters in model (including fixed parameters!).}
    \item{\code{support}:}{Object of class \code{"matrix"},
      range of support for parameters (true parameter scale).}
    \item{\code{params}:}{Object of class \code{"character"},
      parameter names (diagnostic scale).}
    \item{\code{varnames}:}{Object of class \code{"character"},
      parameter names (true parameter scale).}
    \item{\code{fixed}:}{Object of class \code{"nClones"},
      named vactor of fixed parameters.}
    \item{\code{fancy}:}{Object of class \code{"character"},
      fancy model description.}
    \item{\code{backtransf}:}{Object of class \code{"function"},
      function to transform from diagnodstic scale to true parameter
      scale (takes care of fixed value which are not part of the 
      MCMC output.}
    \item{\code{logdensity}:}{Object of class \code{"function"},
      function to calculate log density (used in model selection).}
    \item{\code{neffective}:}{Object of class \code{"function"},
      function to calculate effective sample size from the number 
      of observations.}
  }
}
\section{Methods}{
No methods defined with class "pvamodel" in the signature.
}
\author{
Khurram NAdeem and Peter Solymos
}
\seealso{
\code{\link{pva}}
}
\examples{
showClass("pvamodel")
}
\keyword{classes}

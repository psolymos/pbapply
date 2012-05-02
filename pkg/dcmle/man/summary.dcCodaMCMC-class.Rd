\name{summary.dcCodaMCMC-class}
\Rdversion{1.1}
\docType{class}
\alias{summary.dcCodaMCMC-class}
\alias{show,summary.dcCodaMCMC-method}

\title{Class \code{"summary.dcCodaMCMC"}}
\description{
Summary object.
}
\section{Objects from the Class}{
Objects can be created by calls of the form 
\code{new("summary.dcCodaMCMC", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{settings}:}{Object of class \code{"integer"},
        MCMC settings.}
    \item{\code{coef}:}{Object of class \code{"matrix"},
        coefficients (posterior means).}
    \item{\code{convergence}:}{Object of class \code{"dcDiag"},
        data cloning convergence diagnostics.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{summary.codaMCMC}"}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "summary.dcCodaMCMC")}: ... }
	 }
}
\author{
Peter Solymos
}
\seealso{
\code{\link[dclone]{jags.fit}}, \code{\link[dclone]{dcdiag}}
}
\examples{
showClass("summary.dcCodaMCMC")
}
\keyword{classes}

\name{summary.dcmle-class}
\Rdversion{1.1}
\docType{class}
\alias{summary.dcmle-class}
\alias{show,summary.dcmle-method}

\title{Class \code{"summary.dcmle"}}
\description{
Summary object.
}
\section{Objects from the Class}{
Objects can be created by calls of the form 
\code{new("summary.dcmle", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{title}:}{Object of class \code{"character"},
        title to print, optional.}
    \item{\code{call}:}{Object of class \code{"language"},
        tha call.}
    \item{\code{settings}:}{Object of class \code{"integer"},
        MCMC settings.}
    \item{\code{coef}:}{Object of class \code{"matrix"},
        coefficients (posterior means).}
    \item{\code{convergence}:}{Object of class \code{"dcDiag"},
        data cloning convergence diagnostics.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{summary.dcCodaMCMC}"}, directly.
Class \code{"\linkS4class{summary.codaMCMC}"}, 
by class "summary.dcCodaMCMC", distance 2.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "summary.dcmle")}: ... }
	 }
}
\author{
Peter Solymo
}
\seealso{
\code{\link[dclone]{jags.fit}}, \code{\link[dclone]{dcdiag}}, 
\code{\link{dcmle}}
}
\examples{
showClass("summary.dcmle")
}
\keyword{classes}

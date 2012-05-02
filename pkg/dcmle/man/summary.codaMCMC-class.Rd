\name{summary.codaMCMC-class}
\Rdversion{1.1}
\docType{class}
\alias{summary.codaMCMC-class}
\alias{show,summary.codaMCMC-method}

\title{Class \code{"summary.codaMCMC"}}
\description{
Summary object.
}
\section{Objects from the Class}{
Objects can be created by calls of the 
form \code{new("summary.codaMCMC", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{settings}:}{Object of class \code{"integer"}, 
        MCMC settings.}
    \item{\code{coef}:}{Object of class \code{"matrix"},
        posterior statistics.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "summary.codaMCMC")}: ... }
	 }
}
\author{
Peter Solymos
}
\seealso{
\code{\link[coda]{mcmc.list}}.
}
\examples{
showClass("summary.codaMCMC")
}
\keyword{classes}

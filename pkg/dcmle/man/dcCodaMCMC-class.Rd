\name{dcCodaMCMC-class}
\Rdversion{1.1}
\docType{class}
\alias{dcCodaMCMC-class}
\alias{[,dcCodaMCMC-method}
\alias{[[,dcCodaMCMC-method}
\alias{coerce,dcCodaMCMC,dcmle-method}
\alias{coerce,dcCodaMCMC,MCMClist-method}
\alias{coerce,MCMClist,dcCodaMCMC-method}
\alias{confint,dcCodaMCMC-method}
\alias{dcdiag,dcCodaMCMC-method}
\alias{dctable,dcCodaMCMC-method}
\alias{nclones,dcCodaMCMC-method}
\alias{str,dcCodaMCMC-method}
\alias{summary,dcCodaMCMC-method}

\title{Class \code{"dcCodaMCMC"}}
\description{
An S4 representation of an mcmc.lits object of the \pkg{coda} package,
with data cloning attributes from \pkg{dclone} package
(the mcmc.list.dc class).
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("dcCodaMCMC", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{dctable}:}{Object of class \code{"dcTable"},
        data cloning based iterative posterior statistics
        based on \code{\link[dclone]{dctable}}.}
    \item{\code{dcdiag}:}{Object of class \code{"dcDiag"},
        data cloning convergence diagnostics
        based on \code{\link[dclone]{dcdiag}}.}
    \item{\code{nclones}:}{Object of class \code{"nClones"},
        number of clones.}
    \item{\code{values}:}{Object of class \code{"numeric"},
        same as in \code{"\linkS4class{codaMCMC}"} class.}
    \item{\code{varnames}:}{Object of class \code{"character"},
        same as in \code{"\linkS4class{codaMCMC}"} class.}
    \item{\code{start}:}{Object of class \code{"integer"},
        same as in \code{"\linkS4class{codaMCMC}"} class.}
    \item{\code{end}:}{Object of class \code{"integer"},
        same as in \code{"\linkS4class{codaMCMC}"} class.}
    \item{\code{thin}:}{Object of class \code{"integer"},
        same as in \code{"\linkS4class{codaMCMC}"} class.}
    \item{\code{nchains}:}{Object of class \code{"integer"},
        same as in \code{"\linkS4class{codaMCMC}"} class.}
    \item{\code{niter}:}{Object of class \code{"integer"},
        same as in \code{"\linkS4class{codaMCMC}"} class.}
    \item{\code{nvar}:}{Object of class \code{"integer"},
        same as in \code{"\linkS4class{codaMCMC}"} class.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{codaMCMC}"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "dcCodaMCMC")}: ... }
    \item{[[}{\code{signature(x = "dcCodaMCMC")}: ... }
    \item{coerce}{\code{signature(from = "dcCodaMCMC", to = "dcmle")}: ... }
    \item{coerce}{\code{signature(from = "dcCodaMCMC", to = "MCMClist")}: ... }
    \item{coerce}{\code{signature(from = "dcmle", to = "dcCodaMCMC")}: ... }
    \item{coerce}{\code{signature(from = "MCMClist", to = "dcCodaMCMC")}: ... }
    \item{confint}{\code{signature(object = "dcCodaMCMC")}: ... }
    \item{dcdiag}{\code{signature(x = "dcCodaMCMC")}: ... }
    \item{dctable}{\code{signature(x = "dcCodaMCMC")}: ... }
    \item{nclones}{\code{signature(x = "dcCodaMCMC")}: ... }
    \item{str}{\code{signature(object = "dcCodaMCMC")}: ... }
    \item{summary}{\code{signature(object = "dcCodaMCMC")}: ... }
	 }
}
\author{
Peter Solymos
}
\seealso{
\code{\link[dclone]{jags.fit}}
}
\examples{
showClass("dcCodaMCMC")
}
\keyword{classes}

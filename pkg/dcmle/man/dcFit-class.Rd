\name{dcFit-class}
\Rdversion{1.1}
\docType{class}
\alias{dcFit-class}
\alias{show,dcFit-method}

\title{Class \code{"dcFit"}}
\description{
Compendium for data cloning
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("dcFit", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{multiply}:}{Object of class \code{"dcArgs"},
        same as corresponding \code{\link[dclone]{dc.fit}} argument.}
    \item{\code{unchanged}:}{Object of class \code{"dcArgs"},
        same as corresponding \code{\link[dclone]{dc.fit}} argument.}
    \item{\code{update}:}{Object of class \code{"dcArgs"},
        same as corresponding \code{\link[dclone]{dc.fit}} argument.}
    \item{\code{updatefun}:}{Object of class \code{"dcFunction"},
        same as corresponding \code{\link[dclone]{dc.fit}} argument.}
    \item{\code{initsfun}:}{Object of class \code{"dcFunction"},
        same as corresponding \code{\link[dclone]{dc.fit}} argument.}
    \item{\code{flavour}:}{Object of class \code{"character"},
        same as corresponding \code{\link[dclone]{dc.fit}} argument.}
    \item{\code{data}:}{Object of class \code{"list"},
        same as corresponding \code{\link[dclone]{dc.fit}} argument.}
    \item{\code{model}:}{Object of class \code{"dcModel"},
        same as corresponding \code{\link[dclone]{dc.fit}} argument.}
    \item{\code{params}:}{Object of class \code{"dcParams"},
        same as corresponding \code{\link[dclone]{dc.fit}} argument.}
    \item{\code{inits}:}{Object of class \code{"dcInits"},
        same as corresponding \code{\link[dclone]{dc.fit}} argument.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{gsFit}"}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "dcFit")}: ... }
	 }
}
\author{
Peter Solymos
}
\seealso{
\code{\link[dclone]{dc.fit}}, 
\code{\link{makeDcFit}}
}
\examples{
showClass("dcFit")
}
\keyword{classes}

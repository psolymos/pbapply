\name{gsFit-class}
\Rdversion{1.1}
\docType{class}
\alias{gsFit-class}
\alias{show,gsFit-method}

\title{Class \code{"gsFit"}}
\description{
BUGS/JAGS compendium
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gsFit", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"list"},
        same as corresponding \code{\link[dclone]{jags.fit}} 
        \code{\link[dclone]{bugs.fit}} or argument.}
    \item{\code{model}:}{Object of class \code{"dcModel"},
        same as corresponding \code{\link[dclone]{jags.fit}} 
        \code{\link[dclone]{bugs.fit}} or argument.}
    \item{\code{params}:}{Object of class \code{"dcParams"},
        same as corresponding \code{\link[dclone]{jags.fit}} 
        \code{\link[dclone]{bugs.fit}} or argument.}
    \item{\code{inits}:}{Object of class \code{"dcInits"},
        same as corresponding \code{\link[dclone]{jags.fit}} 
        \code{\link[dclone]{bugs.fit}} or argument.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "gsFit")}: ... }
	 }
}
\author{
Peter Solymos
}
\seealso{
\code{\link[dclone]{jags.fit}}, \code{\link[dclone]{bugs.fit}},
\code{\link{makeGsFit}}
}
\examples{
showClass("gsFit")
}
\keyword{classes}

\name{ricker}
\alias{ricker}
\title{
Rricker growth model
}
\description{
Ricker growth model to be used in model fitting
via \code{\link{pva}}.
}
\usage{
ricker(obs.error = "none", fixed)
}
\arguments{
  \item{obs.error}{
Character, describing the observation error.
Can be \code{"none"}, \code{"poisson"}, or \code{"normal"}.
}
  \item{fixed}{
Named numeric vector with fixed parameter names and values.
}
}
\details{
FIXME
}
\value{
An S4 class of 'pvamodel' (see \code{\link{pvamodel-class}})
}
\author{
Khurram NAdeem and Peter Solymos
}
\seealso{
\code{\link{pvamodel-class}}, \code{\link{pva}}
}
\examples{
ricker()
ricker("poisson")
ricker("normal")
ricker("normal", fixed=c(a=5, sigma=0.5))
}
\keyword{ utils }
\keyword{ models }
\keyword{ ts }

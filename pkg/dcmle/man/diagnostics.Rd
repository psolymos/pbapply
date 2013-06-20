\name{diagnostics}
\alias{gelman.diag}
\alias{gelman.plot}
\alias{geweke.diag}
\alias{heidel.diag}
\alias{raftery.diag}
\title{
Diagnostic functions set as generic
}
\description{
Diagnostic functions set as generic.
}
\usage{
gelman.diag(x, ...)
geweke.diag(x, ...)
heidel.diag(x, ...)
raftery.diag(x, ...)

gelman.plot(x, ...)
}
\arguments{
  \item{x}{
MCMC objects.
}
  \item{\dots}{
Other arguments.
}
}
\details{
Diagnostic functions from \pkg{coda} package are defined
as generics for extensibility.
}
\value{
Diagnostics summaries, and plot.
}
\references{
See relevant help pages.
}
\author{
Peter Solymos
}
\seealso{
\code{\link[coda]{gelman.diag}}
\code{\link[coda]{geweke.diag}}
\code{\link[coda]{heidel.diag}}
\code{\link[coda]{raftery.diag}}

\code{\link[coda]{gelman.plot}}
}
\keyword{utils}

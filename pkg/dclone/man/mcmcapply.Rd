\encoding{UTF-8}
\name{mcmcapply}
\Rdversion{1.1}
\alias{mcmcapply}
\title{
Calculations on 'mcmc.list' objects
}
\description{
Conveniently calculates statistics for mcmc.list objects.
}
\usage{
mcmcapply(x, FUN, ...)
}
\arguments{
  \item{x}{
Objects of class \code{mcmc.list}.
}
  \item{FUN}{
A function to be used in the calculations, returning a single value.
}
  \item{\dots}{
Other arguments passed to \code{FUN}.
}
}
\details{
The \code{mcmcapply} returns a certain statistics based on \code{FUN}.
}
\value{
Returns statistic value based on \code{FUN}, using all values in all chains of the MCMC object.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\examples{
data(regmod)
mcmcapply(regmod, mean)
mcmcapply(regmod, sd)
}
\keyword{ utilities }

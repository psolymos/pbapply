\encoding{UTF-8}
\name{mcmcapply}
\Rdversion{1.1}
\alias{mcmcapply}
\alias{print.mcmc.list}
\title{
Calculations on mcmc.list objects
}
\description{
Conveniently calculates statistics for mcmc.list objects, or combining them.
}
\usage{
mcmcapply(x, FUN, ...)
\method{print}{mcmc.list}(x, ...)
}
\arguments{
  \item{x}{
Objects of class \code{mcmc.list}.
}
  \item{FUN}{
A function to be used in the calculations.
}
  \item{\dots}{
Other arguments passed to \code{FUN}.
}
}
\details{
The \code{mcmcapply} returns a certain statistics based on \code{FUN}.

The \code{print} method prints the object (without possible attributes).
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
}
\keyword{ utilities }

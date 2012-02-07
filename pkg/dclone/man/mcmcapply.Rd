\encoding{UTF-8}
\name{mcmcapply}
\Rdversion{1.1}
\alias{mcmcapply}
\alias{stack.mcmc.list}
\title{
Calculations on 'mcmc.list' objects
}
\description{
Conveniently calculates statistics for mcmc.list objects.
}
\usage{
mcmcapply(x, FUN, ...)
\method{stack}{mcmc.list}(x, ...)
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
\code{mcmcapply} returns a certain statistics based on \code{FUN}
after coercing into a matrix. \code{FUN} can be missing, 
in this case \code{mcmcapply} is equivalent
to calling \code{as.matrix} on an 'mcmc.list' object.

\code{stack} can be used to concatenates 'mcmc.list' 
objects into a single vector
along with index variables indicating where each observation originated
from (e.g. iteration, variable, chain).
}
\value{
\code{mcmcapply} returns statistic value for each variable
based on \code{FUN}, using all values in all chains of the MCMC object.

\code{stack} returns a data frame with columns:
iter, variable, chain, value.
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\examples{
data(regmod)
mcmcapply(regmod, mean)
mcmcapply(regmod, sd)

x <- stack(regmod)
head(x)
summary(x)
library(lattice)
xyplot(value ~ iter | variable, data=x,
    type="l", scales = "free", groups=chain)
}
\keyword{ utilities }

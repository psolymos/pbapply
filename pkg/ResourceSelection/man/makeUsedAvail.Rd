\name{makeUsedAvail}
\alias{makeUsedAvail}
\alias{makeUsedAvail.default}
\alias{makeUsedAvail.formula}
\title{
Make a Used-Aavailable Data Frame
}
\description{
Make a used-available data frame from a presence-absence type data.
}
\usage{
makeUsedAvail(x, ...)

\method{makeUsedAvail}{default}(x, y, ...)

\method{makeUsedAvail}{formula}(formula, data = parent.frame(), ...)
}
\arguments{
  \item{x}{
a matrix or data frame.
}
  \item{y}{
a vector with 0/1 entries, 1s are taken as used observations.
}
  \item{density}{
logical, if shades corresponding to densities should be plotted.
}
  \item{formula}{
two sided model formula of the form \code{y ~ x}.
}
  \item{data}{
data.
}
  \item{\dots}{
other arguments.
}
}
\value{
The function returns a data frame, where used and available portions of 
the input data are bound on top of each other, the first column
refers to \code{y}, where used (1) and available (0) locations are
indicated different from the input values.
All locations in the input data are treated as available (0),
while only nonzero observations in \code{y} are treated as used (1).
}
\author{
Peter Solymos
}
\examples{
(x <- data.frame(species=rep(1:0,each=4), var1=1:8, var2=11:18))
makeUsedAvail(species ~ var1 + var2, x)
}
\keyword{ manip }

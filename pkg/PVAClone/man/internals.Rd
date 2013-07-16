\name{Internals}
\alias{Internals}
\alias{internals}
\alias{PVAClone-internals}
\alias{internal-functions}
\alias{ts_index}
\title{
Internal functions
}
\description{
Functions used internally.
}
\usage{
ts_index(x, type=c("density", "expectation"))
}
\arguments{
  \item{x}{
A vector of observations, possibly with missing values.
}
  \item{type}{
Character, type of index to calculate.
}
}
\details{
\code{ts_index} calculates positional indices of elements of a vector
that fulfill the following conditions when \code{type = "density"}:
(1) if there is only one observation present before the first \code{NA}, 
it is not selected, else, all the observations preceding to the 
first \code{NA} are selected;
(2) if there is only one observation present after the last \code{NA}, 
it is not selected, else, all the observations following the 
last \code{NA} are selected;
(3) if there is only one observation present between two 
consecutive \code{NA}s, it is not selected, else, all the 
observations falling between two consecutive \code{NA}s are selected.
\code{ts_index} calculates positional indices of elements of a vector
that immediately follow a missing (\code{NA}) value if 
\code{type = "expectation"}. The reason for this
is that these elements depend on missing data given a first order
Markov process. As a result, these need different treatment in 
calculating log densities for model selection.
}
\value{
\code{ts_index} returns an integer vector.
}
\author{
Peter Solymos and Khurram Nadeem
}
\examples{
## ts_index
x <- 1:20
x[c(3,4, 6, 10, 13:15, 20)] <- NA
ts_index(x, "density")
ts_index(x, "expectation")
}
\keyword{ utils }
\keyword{ manip }


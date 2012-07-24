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
ts_index(x)
}
\arguments{
  \item{x}{
A vector of observations, possibly with missing values.
}
}
\details{
\code{ts_index} calculates positional indices of elements of a vector
that immediately follow a missing (\code{NA}) value. The reson for this
is that these elements depend on missing data given a first order
Markov process. As a result, these need different treatment in 
calculating log densities for model selection.
}
\value{
\code{ts_index} returns an integer vector.
}
\author{
Peter SOlymos and Khurram Nadeem
}
\examples{
## ts_index
x <- 1:20
x[c(3,4, 6, 10, 13:15, 20)] <- NA
ts_index(x)
}
\keyword{ utils }
\keyword{ manip }

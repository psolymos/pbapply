\name{hbootindex}
\alias{hbootindex}
\title{
Hierarchical bootstrap indices
}
\description{
Generates hierarchical bootstrap indices.
}
\usage{
hbootindex(groups, strata, B = 199)
}
\arguments{
  \item{groups}{
  group membership vector.
}
  \item{strata}{
  strata, optional.
}
  \item{B}{
  number of bootstrap iterations.
}
}
\details{
Resampling with replacement with weights proportional to the number of 
observations in each of the group level (unique values in \code{groups}).

Values of \code{groups} within levels (unique values) of \code{strata}
are resampled independently of other \code{strata} levels.
}
\value{
A matrix with bootstrapped indices, number of columns is \code{B} + 1.
The column is a resample without 
replacement (random subsets can be selected without further
reshuffling). Other elements contain indices according to rules
described in Details section (these also randomly reshuffled).
}
\author{
Peter Solymos
}
\examples{
## equal group sizes
groups <- rep(1:4, each=5)
strata <- rep(1:2, each=10)
hbootindex(groups, strata, 3)

## unequal group sizes
groups <- groups[-c(5,9,10,11)]
strata <- strata[-c(5,9,10,11)]
hbootindex(groups, strata, 3)
}
\keyword{ utils }

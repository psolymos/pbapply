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
Resampling with replacement is done at two levels. The first level
is the group ID level: group IDs (unique values in \code{groups})
are resampled with replacement.

The second level is observations within resampled group ID levels:
observations within these levels are resampled with replacement.

Values of \code{groups} within levels (unique values) of \code{strata}
are resampled independently of other \code{strata} levels.
}
\value{
A list with bootstrapped indices, list length is \code{B} + 1.
The first element has \code{1:length(groups)} samples without 
replacement (random subsets can be selected without further
reshuffling). Other elements contain indices according to rules
described in Details section (these are also randomly reshuffled
to facilitate the creation of random subsets).

Length of resampled vectors are of different lengths when
group sizes are unequal (due to group level resampling).
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

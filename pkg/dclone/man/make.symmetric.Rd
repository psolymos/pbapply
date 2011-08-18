\encoding{UTF-8}
\name{make.symmetric}
\Rdversion{1.1}
\alias{make.symmetric}
\title{
Make a square matrix symmetric by averaging.
}
\description{
Matrix symmetry might depend on numerical precision issues.
There is a bug in JAGS related to this issue for multivariate
normal nodes. This simple function can fix the issue.
}
\usage{
make.symmetric(x)
}
\arguments{
  \item{x}{
A square matrix.
}
}
\details{
The function takes the average of \code{(x[i, j] + x[j, i]) / 2} for each off diagonal cells.
}
\value{
A symmetric square matrix.
}
\note{
The function works for any matrix, even for those not intended to be symmetric.
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\examples{
x <- as.matrix(as.dist(matrix(1:25, 5, 5)))
diag(x) <- 100
x[lower.tri(x)] <- x[lower.tri(x)] - 0.1
x[upper.tri(x)] <- x[upper.tri(x)] + 0.1
x
make.symmetric(x)
}
\keyword{ manip }


\name{splitpb}
\alias{splitpb}
\title{
Divide Tasks for Progress-bar Friendly Distribution in a Cluster
}
\description{
Divides up \code{1:nx} into approximately equal sizes (\code{ncl})
as a way to allocate tasks to nodes in a cluster repeatedly
while updating a progress bar.
}
\usage{
splitpb(nx, ncl)
}
\arguments{
  \item{nx}{
Number of tasks.
}
  \item{ncl}{
Number of cluster nodes.
}
}
\value{
A list of length \code{ceiling(nx / ncl)}, each element being an integer vector
of length \code{ncl} or less.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
Parallel usage of \code{\link{pbapply}} and related functions.
}
\examples{
## define 1 job / worker at a time and repeat
splitpb(10, 4)
## compare this to the no-progress-bar split
## that defines all the jubs / worker up front
parallel::splitIndices(10, 4)
}
\keyword{ utilities }

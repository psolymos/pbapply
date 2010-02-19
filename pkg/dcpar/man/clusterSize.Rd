\name{clusterSize}
\Rdversion{1.1}
\alias{clusterSize}
\alias{plotClusterSize}
\title{
Optimizing the number of workers
}
\description{
The functions help in optimizing workload for the workers if problems are of different size.
}
\usage{
clusterSize(size)
plotClusterSize(n, size, balancing = c("none", "load", "size", "both"),
plot = TRUE, col = NA, xlim = NULL, ylim = NULL, ...)
}
\arguments{
  \item{n}{
Number of workers.
}
  \item{size}{
Vector of problem sizes (recycled if needed).
The default \code{1} indicates equality of problem sizes.
}
  \item{balancing}{
Character, type of balancing to perform, one of \code{c("none", "load", "size", "both")}.
}
  \item{plot}{
Logical, if a plot should be drawn.
}
  \item{col}{
Color of the polygons for work load pieces.
}
  \item{xlim, ylim}{
Limits for the x and the y axis, respectively (optional).
}
  \item{\dots}{
Other arguments passed to \code{\link{polygon}}.
}
}
\details{
These functions help determine the optimal number of workers needed for different sized problems
('size' indicates approximate processing time here). The number of workers needed depends on the
type of balancing.

For the description of the balancing types, see \code{\link{snowWrapper}}.
}
\value{
\code{clusterSize} returns a data frame with approximate processing time as the function of
the number of workers (rows, in \code{1:length(size)}) and the type of balancing
(\code{c("none", "load", "size", "both")}). Approximate processing time is calculated from 
values in \code{size}.

\code{plotClusterSize} invisibly returns the total processing time needed for a setting given
its arguments. As a side effect, a plot is produced (if \code{plot = TRUE}).
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\examples{
## determine the number of workers needed
clusterSize(1:5)
## visually compare balancing options
opar <- par(mfrow=c(2, 2))
plotClusterSize(2,1:5, "none")
plotClusterSize(2,1:5, "load")
plotClusterSize(2,1:5, "size")
plotClusterSize(2,1:5, "both")
par(opar)
}
\keyword{ utilities }

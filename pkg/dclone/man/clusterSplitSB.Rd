\name{clusterSplitSB}
\Rdversion{1.1}
\alias{clusterSplitSB}
\alias{parLapplySB}
\alias{parLapplySLB}
\title{
Size balancing
}
\description{
Functions for size balancing.
}
\usage{
clusterSplitSB(cl, seq, size = 1)
parLapplySB(cl, x, size = 1, fun, ...)
parLapplySLB(cl, x, size = 1, fun, ...)
}
\arguments{
  \item{cl}{
A cluster object created by \code{\link[snow]{makeCluster}}.
}
  \item{x, seq}{
A vector to split.
}
  \item{fun}{
A function or character string naming a function.
}
  \item{size}{
Vector of problem sizes (approximate processing times) corresponding to elements of \code{seq} 
(recycled if needed). The default \code{1} indicates equality of problem sizes.
}
  \item{\dots}{
Other arguments of \code{fun}.
}
}
\details{
\code{clusterSplitSB} splits \code{seq} into subsets, with respect to \code{size}.
In size balancing, the problem is re-ordered from largest to smallest, and then subsets are
determined by minimizing the total approximate processing time.
This splitting is deterministic (reproducible).

\code{parLapplySB} and \code{parLapplySLB} evaluates \code{fun} on elements of \code{x} in parallel, similarly to \code{\link[snow]{parLapply}}. \code{parLapplySB} uses size balancing (\code{clusterSplitSB}).
\code{parLapplySLB} uses size and load balancing. This means that the problem 
is re-ordered from largest to smallest, and then undeterministic load balancing
is used (see \code{\link[snow]{clusterApplyLB}}). If \code{size} is correct, this
is identical to size balancing. This splitting is non-deterministic (might not be reproducible).
}
\value{
\code{clusterSplitSB} returns a list of subsets splitted with respect to \code{size}.

\code{parLapplySB} and \code{parLapplySLB} evaluates \code{fun} on elements of \code{x}, and return a result corresponding to \code{x}. Usually a list with results returned by the cluster.
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
Related functions without size balancing: \code{\link[snow]{clusterSplit}}, \code{\link[snow]{parLapply}}.

Underlying function: \code{\link[snow]{clusterApply}}, \code{\link[snow]{clusterApplyLB}}.

Optimizing the number of workers: \code{\link{clusterSize}}, \code{\link{plotClusterSize}}.
}
\examples{
\dontrun{
cl <- makeSOCKcluster(2)
## equal sizes, same as clusterSplit(cl, 1:5)
clusterSplitSB(cl, 1:5)
## different sizes
clusterSplitSB(cl, 1:5, 5:1)
x <- list(1, 2, 3, 4)
parLapplySB(cl, x, function(z) z^2, size=1:4)
stopCluster(cl)
}
}
\keyword{ utilities }
\keyword{ connection }

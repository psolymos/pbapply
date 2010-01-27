\name{snowWrapper}
\Rdversion{1.1}
\alias{snowWrapper}
\alias{clusterSplitSB}
\alias{clusterSize}
\alias{clusterSeed}
\alias{parLapplySB}
\title{
Convenient functions for parallel MCMC computations
}
\description{
\code{snowWrapper} is a wrapper function around many 'snow' functions,
designed to work closely with MCMC fitting functions (can be called from inside of a function).
}
\usage{
snowWrapper(cl, seq, fun, cldata, name = "cldata", lib = NULL, size = 1,
seed = 1, kind = "default", normal.kind = "default", 
balancing = c("none", "load", "size"), ...)
clusterSplitSB(cl, seq, size = 1)
clusterSize(n, size)
clusterSeed(cl, seed, kind = "default", normal.kind = "default")
parLapplySB(cl, x, size = 1, fun, ...)
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
  \item{cldata}{
A list containing data. This will be assigned to an object \code{name} in the global environment
(an already existing object with same name will be saved and replaced in the end).
This list is then exported to the cluster by \code{\link[snow]{clusterExport}}.
Data in \code{cldata} can be used by \code{fun}.
}
  \item{name}{
Character, the name of \code{cldata} as to be assigned to the global environment and used in \code{fun}.
}
  \item{lib}{
Character, name of package(s). Optionally a package can be loaded onto the cluster. More than one package can be specified as character vector.
}
  \item{balancing}{
Character, type of balancing to perform.
}
  \item{n}{
Maximum number of workers.
}
  \item{size}{
Vector of problem sizes (or relative performance information) correcponding to elements of \code{seq}.
Recicled if needed. The default \code{1} indicates equality of problem sizes.
}
  \item{seed}{
\code{seed} argument used by \code{\link{set.seed}}.
}
  \item{kind}{
\code{kind} argument used by \code{\link{set.seed}}.
}
  \item{normal.kind}{
\code{normal.kind} argument used by \code{\link{set.seed}}.
}
  \item{\dots}{
Other arguments of \code{fun}, that are simple values and not objects. (Arguments passed as objects should be specified in
\code{cldata}, otherwise not exported to the cluster.)
}
}
\details{
The function loads a package \code{lib} onto the cluster, exports \code{cldata} and evaluates \code{fun} on \code{seq}.
Intended mostly for internal use.
}
\value{
Usually a list with results returned by the cluster.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link[snow]{clusterApply}}, \code{\link[snow]{clusterApplyLB}}
}
\examples{
\dontrun{
cl <- makeSOCKcluster(2)
## wrapper
fun <- function(i) cldata$a * i - cldata$b
cldata <- list(a=10, b=5)
snowWrapper(cl, 1:5, fun, cldata)
## split
clusterSplitSB(cl, 1:5)
clusterSplitSB(cl, 1:5, 5:1)
x <- list("a", c("b", "c"), 1, c(2, 3), TRUE)
clusterSplitSB(cl, x)
clusterSplitSB(cl, x, 1:5)
stopCluster(cl)
## setting RNG seeds
cl <- makeSOCKcluster(4)
tmp <- clusterSeed(cl, 1)
res1 <- clusterCall(cl, rnorm, 3)
res1
sapply(2:4, function(i) identical(x[[1]], x[[i]]))
tmp <- clusterSeed(cl, 1:4)
res2 <- clusterCall(cl, rnorm, 3)
res2
sapply(2:4, function(i) identical(x[[1]], x[[i]]))
tmp <- clusterSeed(cl, 1)
res3 <- clusterCall(cl, rnorm, 3)
res3
identical(res1, res3) # same seed, same results
stopCluster(cl)
## determine the number of workers
clusterSize(5, rep(1, 5))
clusterSize(5, 1:5)
}
}
\keyword{ utilities }
\keyword{ connection }

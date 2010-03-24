\name{snowWrapper}
\Rdversion{1.1}
\alias{snowWrapper}
\title{
Parallel wrapper function to call from within a function
}
\description{
\code{snowWrapper} is a wrapper function around many \pkg{snow} and other \pkg{dcpar} functions,
designed to work closely with MCMC fitting functions (can be called from inside of a function).
}
\usage{
snowWrapper(cl, seq, fun, cldata, name = "cldata", lib = NULL, evalq=NULL,
size = 1, seed = 1, kind = "default", normal.kind = "default", 
balancing = c("none", "load", "size", "both"), dir = getwd(), ...)
}
\arguments{
  \item{cl}{
A cluster object created by \code{\link[snow]{makeCluster}}.
}
  \item{seq}{
A vector to split.
}
  \item{fun}{
A function or character string naming a function.
}
  \item{cldata}{
A list containing data. This will be assigned to an object \code{name} in the global environment
(an already existing object with same name will be saved and replaced back in the end).
This list is then exported to the cluster by \code{\link[snow]{clusterExport}}.
Data in \code{cldata} can be used by \code{fun}.
}
  \item{name}{
Character, the name of \code{cldata} as to be assigned to the global environment and used in \code{fun}.
}
  \item{lib}{
Character, name of package(s). Optionally packages can be loaded onto the cluster. More than one package can be specified as character vector.
}
  \item{evalq}{
Character, expressions to evaluate, e.g. for changing global options (passed to \code{\link[snow]{clusterEvalQ}}).
More than one expressions can be specified as character vector.
}
  \item{balancing}{
Character, type of balancing to perform (see Details).
}
  \item{size}{
Vector of problem sizes (or relative performance information) corresponding to elements of \code{seq} (recycled if needed). The default \code{1} indicates equality of problem sizes.
}
  \item{seed}{
\code{seed} argument used by \code{\link{clusterSeed}}.
}
  \item{kind}{
\code{kind} argument used by \code{\link{clusterSeed}}.
}
  \item{normal.kind}{
\code{normal.kind} argument used by \code{\link{clusterSeed}}.
}
  \item{dir}{
Working directory to use.
}
  \item{\dots}{
Other arguments of \code{fun}, that are simple values and not objects. (Arguments passed as objects should be specified in \code{cldata}, otherwise not exported to the cluster.)
}
}
\details{
The function sets the random seeds, loads packages \code{lib} onto the cluster, sets the working directory as \code{dir}, exports \code{cldata} and evaluates \code{fun} on \code{seq}.

No balancing (\code{balancing = "none"}) means, that the problem is splitted into roughly equal
subsets, without respect to \code{size} (see \code{\link[snow]{clusterSplit}}). This splitting
is deterministic (reproducible).

Load balancing (\code{balancing = "load"}) means, that the problem is not splitted into subsets 
\emph{a priori}, but subsequent items are placed on the worker which is empty 
(see \code{\link[snow]{clusterApplyLB}} for load balancing). This splitting
is non-deterministic (might not be reproducible).

Size balancing (\code{balancing = "size"}) means, that the problem is splitted into 
subsets, with respect to \code{size} (see \code{\link{clusterSplitSB}} and \code{\link{parLapplySB}}).
In size balancing, the problem is re-ordered from largest to smallest, and then subsets are
determined by minimizing the total approximate processing time.
This splitting is deterministic (reproducible). 

Size and load balancing (\code{balancing = "both"}) means, that the problem 
is re-ordered from largest to smallest, and then undeterministic load balancing
is used (see \code{\link{parLapplySLB}}). If \code{size} is correct, this
is identical to size balancing. This splitting is non-deterministic (might not be reproducible).
}
\value{
Usually a list with results returned by the cluster.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
Size balancing: \code{\link{parLapplySB}}, \code{\link{parLapplySLB}}

Optimizing the number of workers: \code{\link{clusterSize}}, \code{\link{plotClusterSize}}.

\code{snowWrapper} is used internally by \code{\link{jags.parfit}} and \code{\link{dc.parfit}}.
}
\examples{
\dontrun{
cl <- makeSOCKcluster(2)
## wrapper
fun <- function(i) cldata$a * i - cldata$b
cldata <- list(a=10, b=5)
snowWrapper(cl, 1:5, fun, cldata)
stopCluster(cl)
}
}
\keyword{ utilities }
\keyword{ connection }

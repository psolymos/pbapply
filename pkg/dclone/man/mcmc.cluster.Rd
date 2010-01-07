\name{mcmc.cluster}
\Rdversion{1.1}
\alias{mcmc.cluster}
\title{
Convenient function for parallel MCMC computations
}
\description{
A function similar to \code{\link[snow]{clusterApply}} and \code{\link[snow]{clusterApplyLB}},
designed to work closely with MCMC fitting functions (can be called from inside of a function).
}
\usage{
mcmc.cluster(cl, seq, fun, cldata, name = "cldata", 
lib = NULL, load.balancing = TRUE, ...)
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
  \item{load.balancing}{
Logical, if \code{TRUE} the function \code{\link[snow]{clusterApplyLB}} is used istead of \code{\link[snow]{clusterApply}}.
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
A list with results returned by the cluster.
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
fun <- function(i) cldata$a * i - cldata$b
cldata <- list(a=10, b=5)
mcmc.cluster(cl, 1:2, fun, cldata)
stopCluster(cl)
}
}
\keyword{ utilities }
\keyword{ connection }

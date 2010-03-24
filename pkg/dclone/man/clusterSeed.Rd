\name{clusterSeed}
\Rdversion{1.1}
\alias{clusterSeed}
\title{
Parallel random seeds and kinds
}
\description{
Sets the seeds and kinds for random numbers on parallel workers.
}
\usage{
clusterSeed(cl, seed, kind = "default", normal.kind = "default")
}
\arguments{
  \item{cl}{
A cluster object created by \code{\link[snow]{makeCluster}}.
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
}
\value{
Sets rendom seeds and kinds, returns the settings as a list.
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
## seed not set
clusterCall(cl, rnorm, 3)
## different seeds
clusterSeed(cl, 1:2)
clusterCall(cl, rnorm, 3)
## same seeds
clusterSeed(cl, 1)
clusterCall(cl, rnorm, 3)
stopCluster(cl)
}
}
\keyword{ utilities }
\keyword{ connection }

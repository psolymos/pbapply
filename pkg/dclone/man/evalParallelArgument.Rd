\name{evalParallelArgument}
\alias{evalParallelArgument}
\title{
Evaluates parallel argument
}
\description{
Evaluates parallel argument.
}
\usage{
evalParallelArgument(cl, quit = FALSE)
}
\arguments{
  \item{cl}{
\code{NULL}, a cluster object or an integer. Can be missing.
}
  \item{quit}{
Logical, whether it should stop with error when ambiguous
parallel definition is found (conflicting default 
environmental variable settings).
}
}
\value{
\code{NULL} for sequential evaluation or
the original value of \code{cl} if parallel
evaluation is meaningful.
}
\author{
Peter Solymos
}
\examples{
evalParallelArgument()
evalParallelArgument(NULL)
evalParallelArgument(1)
evalParallelArgument(2)
cl <- makePSOCKcluster(2)
evalParallelArgument(cl)
stopCluster(cl)
oop <- options("mc.cores"=2)
evalParallelArgument()
options(oop)
}
\keyword{ misc }
\keyword{ utilities }

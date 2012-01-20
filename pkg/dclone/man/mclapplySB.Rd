\name{mclapplySB}
\alias{mclapplySB}
\title{
  Size balancing version of mclapply
}
\description{
  \code{mclapplySB} is a size balancing version of \code{\link[parallel]{mclapply}}.
}
\usage{
mclapplySB(X, FUN, ..., 
    mc.preschedule = TRUE, mc.set.seed = TRUE,
    mc.silent = FALSE, mc.cores = 1L,
    mc.cleanup = TRUE, mc.allow.recursive = TRUE, 
    size = 1)
}
\arguments{
\item{X}{a vector (atomic or list) or an expressions vector.  Other
  objects (including classed objects) will be coerced by
  \code{\link{as.list}}.}
\item{FUN}{the function to be applied to each element of \code{X}}
\item{...}{optional arguments to \code{FUN}}
\item{mc.preschedule}{see \code{\link[parallel]{mclapply}}}
\item{mc.set.seed}{see \code{\link[parallel]{mclapply}}}
\item{mc.silent}{see \code{\link[parallel]{mclapply}}}
\item{mc.cores}{The number of cores to use, i.e. how many processes
  will be spawned (at most)}
\item{mc.cleanup}{see \code{\link[parallel]{mclapply}}}
\item{mc.allow.recursive}{see \code{\link[parallel]{mclapply}}}
\item{size}{Vector of problem sizes (or relative performance information) 
  corresponding to elements of \code{X} (recycled if needed). 
  The default \code{1} indicates equality of problem sizes.
}
}
\value{
 A list.
}
\details{
\code{\link[parallel]{mclapply}} gives details of the forking
mechanism. 

\code{\link[parallel]{mclapply}} is used unmodified 
if sizes of the jobs are equal (\code{length(unique(size)) == 1}).
Size balancing (as described in \code{\link{snowWrapper}})
is used to balance workload on the child processes
otherwise.
}
\seealso{
  \code{\link[parallel]{mclapply}}, \code{\link{snowWrapper}}
}
\author{Peter Solymos}
\keyword{interface}

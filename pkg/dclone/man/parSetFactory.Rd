\name{parSetFactory}
\alias{parListFactories}
\alias{parSetFactory}
\title{Advanced control over JAGS on parallel workers}
\description{
  JAGS modules contain factory objects for samplers, monitors, and
  random number generators for a JAGS model.  These functions allow
  fine-grained control over which factories are active
   on parallel workers.
}
\usage{
parListFactories(cl, type)
parSetFactory(cl, name, type, state)
}
\arguments{
  \item{cl}{a cluster object created by the 
    \pkg{parallel} (or \pkg{snow}) package.}
  \item{name}{name of the factory to set}
  \item{type}{type of factory to query or set. Possible values are
    \code{"sampler"}, \code{"monitor"}, or \code{"rng"}}
  \item{state}{a logical. If \code{TRUE} then the factory will be
    active, otherwise the factory will become inactive.}
}
\value{
  \code{parListFactories} returns a a list of data frame 
  with two columns per each worker, the first
  column shows the names of the factory objects in the currently loaded
  modules, and the second column is a logical vector indicating whether
  the corresponding factory is active or not.

  \code{sparStFactory} is called to change 
  the future behaviour of factory
  objects. If a factory is set to inactive then it will be skipped.
} 
\note{
  When a module is loaded, all of its factory objects are active. This
  is also true if a module is unloaded and then reloaded.
}
\author{Peter Solymos, \email{solymos@ualberta.ca}}
\seealso{\code{\link[rjags]{list.modules}}, 
\code{\link[rjags]{set.factory}}
}
\examples{
\dontrun{
cl <- makePSOCKcluster(3)
parListFactories(cl, "sampler")
parListFactories(cl, "monitor")
parListFactories(cl, "rng")
parSetFactory(cl, "base::Slice", "sampler", FALSE)
parListFactories(cl, "sampler")
parSetFactory(cl, "base::Slice", "sampler", TRUE)
stopCluster(cl)
}
}
\keyword{models}

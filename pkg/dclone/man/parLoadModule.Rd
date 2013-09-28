\name{parLoadModule}
\alias{parLoadModule}
\alias{parUnloadModule}
\alias{parListModules}
\title{Dynamically load JAGS modules on parallel workers}
\description{
  A JAGS module is a dynamically loaded library that extends the
  functionality of JAGS. These functions load and unload JAGS modules
  and show the names of the currently loaded modules on parallel workers.
}
\usage{
parLoadModule(cl, name, path, quiet=FALSE)
parUnloadModule(cl, name, quiet=FALSE)
parListModules(cl)
}
\arguments{
  \item{cl}{a cluster object created by the 
    \pkg{parallel} (or \pkg{snow}) package.}
  \item{name}{character, name of the module to be loaded}
  \item{path}{file path to the location of the DLL. If omitted,
    the option \code{jags.moddir} is used to locate the modules.
    it can be a vector of length \code{length(cl)} to set different
    DLL locations on each worker}
  \item{quiet}{a logical. If \code{TRUE}, no message will be printed
    about loading the module}
}
\author{Peter Solymos, \email{solymos@ualberta.ca}}
\seealso{\code{\link[rjags]{list.modules}},
\code{\link[rjags]{load.module}}, \code{\link[rjags]{unload.module}}}
\examples{
\dontrun{
if (require(rjags)) {
cl <- makePSOCKcluster(3)
parListModules(cl)
parLoadModule(cl, "glm")
parListModules(cl)
parUnloadModule(cl, "glm")
parListModules(cl)
stopCluster(cl)
}
}
}
\keyword{interface}


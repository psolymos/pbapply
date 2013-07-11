\encoding{UTF-8}
\name{snowWrapper}
\Rdversion{1.1}
\alias{snowWrapper}
\title{
Parallel wrapper function to call from within a function
}
\description{
This function is now replaced by \code{\link{parDosa}}.
}
\usage{
snowWrapper(cl, seq, fun,  
    cldata, name = "cldata", use.env=FALSE, 
    lib = NULL, dir = NULL, evalq=NULL,
    size = 1, balancing = c("none", "load", "size", "both"), 
    rng.type = c("none", "RNGstream", "SPRNG"), 
    cleanup = TRUE, unload = FALSE, envir = .GlobalEnv, ...)
}
\arguments{
  \item{cl}{
A cluster object created by \code{\link[snow]{makeCluster}}, or
an integer. It can also be \code{NULL}, see Details.
}
  \item{seq}{
A vector to split.
}
  \item{fun}{
A function or character string naming a function.
}
  \item{cldata}{
A list containing data. This will be assigned to 
an object \code{name} in the global environment
(an already existing object with same name will be 
saved and replaced back in the end).
This list is then exported to the cluster by 
\code{\link[snow]{clusterExport}}.
Data in \code{cldata} can be used by \code{fun}.
}
  \item{name}{
Character, the name of \code{cldata} as to be assigned 
to the global environment and used in \code{fun}.
}
  \item{use.env}{
Logical, if \code{name} is to be treated as a 
list object or an environment.
}
  \item{lib}{
Character, name of package(s). Optionally packages can be 
loaded onto the cluster. More than one package can be 
specified as character vector. Packages already loaded are skipped.
}
  \item{dir}{
Working directory to use, if \code{NULL} working 
directory is not set on workers (default).
Can be a vector to set different directories on workers.
}
  \item{evalq}{
Character, expressions to evaluate, 
e.g. for changing global options (passed to \code{\link[snow]{clusterEvalQ}}).
More than one expressions can be specified as character vector.
}
  \item{balancing}{
Character, type of balancing to perform (see Details).
}
  \item{size}{
Vector of problem sizes (or relative performance information) 
corresponding to elements of \code{seq} (recycled if needed). 
The default \code{1} indicates equality of problem sizes.
}
  \item{rng.type}{
Character, \code{"none"} or the type of RNG on the workers 
(see \code{\link[snow]{clusterSetupRNG}}).
The logical value \code{!(rng.type == "none")} is used for 
forking (e.g. when \code{cl} is integer).
}
  \item{cleanup}{
logical, if \code{cldata} should be removed from 
the workers after applying \code{fun}.
If \code{TRUE}, effects of \code{dir} argument is also cleaned up.
}
  \item{unload}{
logical, if \code{pkg} should be unloaded after applying \code{fun}.
}
  \item{envir}{
environment to use when assigning data values.
}
  \item{\dots}{
Other arguments of \code{fun}, that are simple values and not objects. 
(Arguments passed as objects should be specified in \code{cldata}, 
otherwise those are not exported to the cluster by this function.)
}
}
\value{
Usually a list with results returned by the cluster.
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{
This function is now replaced by \code{\link{parDosa}}.
}
\keyword{ utilities }
\keyword{ connection }

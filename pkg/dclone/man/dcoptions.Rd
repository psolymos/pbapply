\name{dcoptions}
\alias{dcoptions}
\title{
Setting Options
}
\description{
Setting options.
}
\usage{
dcoptions(...)
}
\arguments{
  \item{\dots}{
Arguments in \code{tag = value} form, or a list of tagged values.
The tags must come from the parameters described below.
}
}
\details{
\code{dcoptions} is a convenient way of handling options related to the 
package.
}
\value{
When parameters are set by \code{dcoptions}, their former values are 
returned in an invisible named list. Such a list can be passed as an 
argument to \code{dcoptions} to restore the parameter values.
Tags are the following:

  \item{rhat}{critical value for testing chain convergence, default is 1.1.}
  \item{autoburnin}{logical, to use in \code{\link{gelman.diag}} (default is \code{TRUE}).}
  \item{diag}{critical value to use for data cloning convergence diagnostics, default is 0.05.}
  \item{verbose}{numeric, should output be verbose (>0) or not (0), default is 1.}
  \item{LB}{logical, should load balancing be used, default is \code{FALSE}.}
  \item{RNG}{parallel RNG type, either 
    \code{"none"} (default), \code{"RNGstream"} or \code{"SPRNG"},
    see \code{\link[snow]{clusterSetupRNG}}.}
  \item{single.par.model}{logical, if a single model file should be written to hard drive
    during parallel computations (e.g. when memory is shared), or not (e.g. for computing
    grids and clusters). Default is \code{TRUE}. If \code{FALSE}, \code{length(cl)} files
    are created, one at each worker.}
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\examples{
getOption("dcoptions")
ov <- dcoptions(LB=TRUE)
ov
getOption("dcoptions")
}
\keyword{ utilities }

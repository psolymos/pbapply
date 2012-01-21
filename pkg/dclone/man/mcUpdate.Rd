\name{mcUpdate}
\alias{mcUpdate}
\title{Update jags models on multicore workers}
\description{
  Update the Markov chain associated with the model on parallel workers. 
}
\usage{
mcUpdate(object, n.iter=1, \ldots)
}
\arguments{
  \item{object}{an \code{mc.jags} model object}
  \item{n.iter}{number of iterations of the Markov chain to run}
  \item{\ldots}{additional arguments to the update method, see
    \code{\link[rjags]{update.jags}}}
}
\value{
  The \code{mcUpdate} function modifies the
  original object and returns \code{NULL} on parallel workers.
}
\author{Peter Solymos, \email{solymos@ualberta.ca}}
\seealso{\code{\link[rjags]{update.jags}}

See example on help page of \code{\link{mcCodaSamples}}.}
\keyword{models}

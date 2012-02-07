\name{jagsModel}
\alias{jagsModel}
\title{Create a JAGS model object}
\description{
\code{jagsModel} is used to create an object representing a
Bayesian graphical model, specified with a BUGS-language description
of the prior distribution, and a set of data.
This function uses \code{\link[rjags]{jags.model}} but keeps track
of data cloning information supplied via the \code{data} argument.
The model argument can also accept functions or 'custommodel' objects.

}
\usage{
jagsModel(file, data=sys.frame(sys.parent()), inits, n.chains = 1,
    n.adapt=1000, quiet=FALSE)}
\arguments{
\item{file}{the name of the file containing a 
description of the model in the
JAGS dialect of the BUGS language.
Alternatively, \code{file} can be a readable text-mode connection,
or a complete URL. It can be also a function or a 
\code{\link{custommodel}} object.}
\item{data}{a list or environment containing the data. Any numeric
objects in \code{data} corresponding to node arrays used in
\code{file} are taken to represent the values of observed nodes
in the model}
\item{inits}{optional specification of initial values in the form of a
list or a function. If omitted,
initial values will be generated automatically. It is an error to
supply an initial value for an observed node.}
\item{n.chains}{the number of chains for the model}
\item{n.adapt}{the number of iterations for adaptation. See
\code{\link[rjags]{adapt}} for details. If \code{n.adapt = 0} then no
adaptation takes place.}
\item{quiet}{if \code{TRUE} then messages generated during compilation
will be suppressed.}
}
\value{
\code{parJagsModel} returns an object inheriting from class \code{jags}
which can be used to generate dependent samples from the posterior
distribution of the parameters.

An object of class \code{jags} is a list of functions that share a
common environment, see \code{\link[rjags]{jags.model}} for details.

An \code{n.clones} attribute is attached to the object when applicable.
}

\author{Peter Solymos, \email{solymos@ualberta.ca}}
\seealso{
Underlying functions: \code{\link[rjags]{jags.model}}, 
\code{\link[rjags]{update.jags}}

See example on help page of \code{\link{codaSamples}}.

Parallel version: \code{\link{parJagsModel}}
}
\keyword{models}


\encoding{UTF-8}
\name{parallel.inits}
\Rdversion{1.1}
\alias{parallel.inits}
\title{
Parallel RNGs for initial values
}
\description{
This function takes care of initial values with safe RNGs based on
\code{\link[rjags]{parallel.seeds}} of the \pkg{rjags} package.
}
\usage{
parallel.inits(inits, n.chains)
}
\arguments{
  \item{inits}{
Initial values (see Initialization at \code{\link[rjags]{jags.model}}). 
If \code{NULL}, an empty list of length \code{n.chains}
will be generated and seeded (RNG type and seed).
}
  \item{n.chains}{
Number of chains to generate.
}
}
\details{
Initial values are handled similar to as it is done in
\code{\link[rjags]{jags.model}}.

RNGs are based on values returned by 
\code{\link[rjags]{parallel.seeds}}.

If the \code{"lecuyer"} JAGS module is active, RNGs are based on
the \code{"lecuyer::RngStream"} factory, otherwise those are based on
the \code{"base::BaseRNG"} factory.
}
\value{
Returns a list of initial values with RNGs.
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}. Based on Martyn Plummer's 
\code{\link[rjags]{parallel.seeds}} function and code in 
\code{\link[rjags]{jags.model}} for initial value handling in the 
\pkg{rjags} package.
}
\seealso{
\code{\link[rjags]{parallel.seeds}}, \code{\link[rjags]{jags.model}}

This seeding function is used in all of \pkg{dclone}'s 
parallel functions that do initialization: 
\code{\link{parJagsModel}}, \code{\link{jags.parfit}}, 
\code{\link{dc.parfit}}
}
\examples{
## "base::BaseRNG" factory.
parallel.inits(NULL, 2)
## "lecuyer::RngStream" factory
load.module("lecuyer")
parallel.inits(NULL, 2)
unload.module("lecuyer")
## some non NULL inits specifications
parallel.inits(list(a=0), 2)
parallel.inits(list(list(a=0), list(a=0)), 2)
parallel.inits(function() list(a=0), 2)
parallel.inits(function(chain) list(a=chain), 2)
}
\keyword{ utilities }

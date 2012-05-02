\name{makeDcFit}
\alias{makeDcFit}
\alias{makeGsFit}
\title{
Data object creators
}
\description{
Creator functions for data types used in the \pkg{dcmle} package.
}
\usage{
makeGsFit(data, model, params = NULL, inits = NULL)

makeDcFit(data, model, params=NULL, inits = NULL,
    multiply = NULL, unchanged = NULL, update = NULL,
    updatefun = NULL, initsfun = NULL, flavour)
}
\arguments{
  \item{data}{usually a named list with data.}
  \item{model}{BUGS model (function, character vector or
    a \code{\link[dclone]{custommodel}} object).
    The argument is coerced into a \code{custommodel} object.}
  \item{params}{optional, character vector for model
    parameters to monitor.}
  \item{inits}{initial values (\code{NULL}, list or function).}
  \item{multiply}{optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{unchanged}{optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{update}{optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{updatefun}{optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{initsfun}{optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{flavour}{optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
}
\details{
'gsFit' (after BU*GS*/JA*GS*) is a basic object class representing 
requirements for the Bayesian MCMC model fitting.
The 'dcFit' object class extends 'gsFit'
by additional slots that are used to fine tune
how data cloning is done during fitting process.
Both 'gsFit' and 'dcFit' represent prerequisites
for model fitting, but do not containing any fitted
parts. Creator functions \code{makeGsFit} and \code{makeDcFit}
are available for these classes.
See \code{\link{dcmle-package}} help page for usage of creator functions.

The default \code{flavour} is stored in 
\code{getOption("dcmle.flavour")} with value \code{"jags"}.
It can be changed as \code{options("dcmle.flavour"="bugs")}
if required.
}
\value{
\code{makeGsFit} returns a 'gsFit' object (\link{gsFit-class}).

\code{makeDcFit} returns a 'dcFit' object (\link{dcFit-class}).
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\link{gsFit-class}, \link{dcFit-class}, \code{\link{dcmle}}
}
\examples{
showClass("gsFit")
new("gsFit")
showClass("dcFit")
new("dcFit")
}
\keyword{ classes }
\keyword{ utils }

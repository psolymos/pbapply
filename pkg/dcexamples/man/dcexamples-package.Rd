\name{dcexamples-package}
\alias{dcexamples-package}
\alias{dcexamples}
\docType{package}
\title{
Hierarchical Model Examples Using Data Cloning
}
\description{
The package contains example data sets
that can be used as templates in fitting
models to data using data cloning.

Examples are built on the infrastructure
provided by the \pkg{dclone} and
\pkg{dcmle} packages.

Examples are developed for JAGS.
Running them under WinBUGS or OpenBUGS
might require modification,
and the \code{@flavour} slot to be changed
to \code{"bugs"}.
}
\author{
Peter Solymos

Maintainer: Peter Solymos <solymos@ualberta.ca>
}
\references{
\url{http:/dcr.r-forge.r-project.org}
}
\keyword{ package }
\seealso{
Main function the this package: \code{\link{dcexample}}.
}
\examples{
\dontrun{
## index of currently available exampes
help(package="dcexamples")
}

## how to use an example as a template
## paramecium example
}

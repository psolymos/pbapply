\name{dcpar-package}
\alias{dcpar-package}
\alias{dcpar}
\docType{package}
\title{
dcpar: Parallel Computing with MCMC and Data Cloning
}
\description{
The \pkg{dcpar} package is a parallel computing extension of the \pkg{dclone} package
built on the infrastructure provided by the \pkg{snow} package.
}
\details{
\code{\link{jags.parfit}}: parallell version of the \code{\link[dclone]{jags.fit}} function,
it runs MCMC chains on parallel workers.

\code{\link{dc.parfit}}: parallell version of the \code{\link[dclone]{dc.fit}} function,
it runs different models on parallel workers, by using size balancing to minimize the
processing time.

\code{\link{snowWrapper}}: an internal function that can be called from within functions to prepare for
parallel executions.

\code{\link{parLapplySB}}: size balancing version of the \code{\link[snow]{parLapply}} function.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}

Maintainer: \enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\references{
Nothing at the moment.
}
\keyword{ package }
\seealso{
\code{\link[dclone:dclone-package]{dclone}}
}

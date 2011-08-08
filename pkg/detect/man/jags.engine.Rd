\name{jags.engine}
\alias{jags.engine}
\title{
Function used for data cloning
}
\description{
This is used for data cloning, it call the JAGS MCMC software.
}
\usage{
jags.engine(data, params, model, inits = NULL, cl = NULL, ...)
}
\arguments{
  \item{data}{
data as a list
}
  \item{params}{
parameters to monitor
}
  \item{model}{
model in BUGS language
}
  \item{inits}{
initial values
}
  \item{cl}{
cluster object for parallel computations
}
  \item{\dots}{
other arguments
}
}
\value{
An MCMC object
}
\references{
Solymos dclone
}
\author{
Peter Solymos
}
\keyword{ htest }

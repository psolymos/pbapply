\name{gompertz}
\alias{gompertz}
\title{
Gompertz growth model
}
\description{
Gompertz growth model to be used in model fitting
via \code{\link{pva}}.
}
\usage{
gompertz(obs.error = "none", fixed)
}
\arguments{
  \item{obs.error}{
Character, describing the observation error.
Can be \code{"none"}, \code{"poisson"}, or \code{"normal"}.
}
  \item{fixed}{
Named numeric vector with fixed parameter names and values.
}
}
\details{
The function can be called in pva to fit the Gompertz growth model 
to a given population time series assuming both
with and without observation error. When assuming the presence of 
observation error, either the Normal 
or the Poisson observation error model must be assumed within the 
state-space model formulation (Nadeem and Lele, 2012).

The Gompertz growth model and the two observation error models are 
defined as follows:

Gompertz Model: 
\deqn{x_{t} = a + x_{t-1} + b x_{t-1} + \epsilon_{t}}{%
x[t] = a + x[t-1] + bx[t-1] + E[t]}
where \eqn{x_{t}}{x[t]} is log abundance at time \eqn{t} and 
\eqn{\epsilon_{t} \sim Normal(0, \sigma^2)}{E[t] \~ Normal(0, sigma^2)}.

Observation error models are described in the help page of 
\code{\link{pva}}.

The argument 'fixed' can be used to fit the model assuming 
a priori values of a subset of the parameters.
The number of parameters that should be fixed at most is \eqn{p-1}, 
where \eqn{p} is the dimension of the full model. See examples
below and in \code{\link{pva}} and \code{\link{model.select}}.
}
\value{
An S4 class of 'pvamodel' (see \code{\link{pvamodel-class}})
}
\references{
Nadeem, K., Lele S. R., 2012.  
Likelihood based population viability analysis in the presence of 
observation error. Oikos. doi: 10.1111/j.1600-0706.2011.20010.x
}
\author{
Khurram Nadeem and Peter Solymos
}
\seealso{
\code{\link{pvamodel-class}}, \code{\link{pva}}
}
\examples{
gompertz()
gompertz("poisson")
gompertz("normal")
gompertz("normal", fixed=c(a=5, sigma=0.5))
}
\keyword{ utils }
\keyword{ models }
\keyword{ ts }

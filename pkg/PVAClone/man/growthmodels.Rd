\name{growth-models}
\alias{growthmodels}
\alias{gompertz}
\alias{ricker}
\alias{bevertonholt}
\alias{thetalogistic}
\alias{thetalogistic_D}
\title{
Growth models
}
\description{
Population growth model to be used in model fitting
via \code{\link{pva}}.
}
\usage{
gompertz(obs.error = "none", fixed)
ricker(obs.error = "none", fixed)
thetalogistic(obs.error = "none", fixed)
thetalogistic_D(obs.error = "none", fixed)
bevertonholt(obs.error = "none", fixed)
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
These functions can be called in \code{\link{pva}} to fit the following 
growth models to a given population time series assuming both
with and without observation error. When assuming the presence of 
observation error, either the Normal 
or the Poisson observation error model must be assumed within the 
state-space model formulation (Nadeem and Lele, 2012). The growth
models are defined as follows.

Gompertz (\code{gompertz}):
\deqn{x_{t} = a + x_{t-1} + b x_{t-1} + \epsilon_{t}}{%
x[t] = a + x[t-1] + bx[t-1] + E[t]}
where \eqn{x_{t}}{x[t]} is log abundance at time \eqn{t} and 
\eqn{\epsilon_{t} \sim Normal(0, \sigma^2)}{E[t] ~ Normal(0, sigma^2)}.

Ricker (\code{ricker}):
\deqn{x_{t} = x_{t-1} + a + b e^{x_{t-1}} + \epsilon_{t}}{%
x[t] = x[t-1] + a + be^(x[t-1]) + E[t]}
where \eqn{x_{t}}{x[t]} is log abundance at time 
\eqn{t} and \eqn{\epsilon_{t} \sim Normal(0, \sigma^2}{%
E[t] ~ Normal(0, sigma^2)}.

Theta-Logistic (\code{thetalogistic}):
\deqn{x_{t} = x_{t-1} + r[1-(e^{x_{t-1}}/K)^theta] + \epsilon_{t}}{%
x[t] = x[t-1] + r[1-(e^(x[t-1])/K)^theta] + E[t]}
where \eqn{x_{t}}{x[t]} is log abundance at time 
\eqn{t} and \eqn{\epsilon_{t} \sim Normal(0, \sigma^2}{%
E[t] ~ Normal(0, sigma^2)}.

Theta-Logistic with Demographic Variability
(\code{thetalogistic_D}):
\deqn{x_{t} = x_{t-1} + r[1-(e^{x_{t-1}}/K)^theta] + \epsilon_{t}}{%
x[t] = x[t-1] + r[1-(e^(x[t-1])/K)^theta] + E[t]}
where \eqn{x_{t}}{x[t]} is log abundance at time 
\eqn{t} and \eqn{\epsilon_{t} \sim Normal(0, \sigma^2 + sigma.d^2}{%
E[t] ~ Normal(0, sigma^2+sigma.d^2)}, where \eqn{sigma.d^2}{sigma.d^2}
is the demographic variability. If \eqn{sigma.d^2}{sigma.d^2} is 
missing or fixed at zero, Theta-Logistic model is fitted instead.

Generilzed Beverton-Holt (\code{bevertonholt}):
\deqn{x_{t} = x_{t-1} + r- log[1+(e^{x_{t-1}}/K)^theta] + \epsilon_{t}}{%
x[t] = x[t-1] + r-[1+(e^(x[t-1])/K)^theta] + E[t]}
where \eqn{x_{t}}{x[t]} is log abundance at time 
\eqn{t} and \eqn{\epsilon_{t} \sim Normal(0, \sigma^2}{%
E[t] ~ Normal(0, sigma^2)}.

Observation error models are described in the help page of 
\code{\link{pva}}.

The argument \code{fixed} can be used to fit the model assuming 
a priori values of a subset of the parameters. For instance,
fixing theta equal to one reduces Theta-Logistic and 
Gnerelized Beverton-Holt models to Logistic and Beverton-Holt 
models respectively. The number of parameters that should be 
fixed at most is \eqn{p-1}, where \eqn{p} is the dimension of 
the full model. See examples below and in \code{\link{pva}} 
and \code{\link{model.select}}.
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
ricker("normal")
ricker("normal", fixed=c(a=5, sigma=0.5))
thetalogistic("none", fixed=c(theta=1))
bevertonholt("normal", fixed=c(theta=1))
}
\keyword{ utils }
\keyword{ models }
\keyword{ ts }

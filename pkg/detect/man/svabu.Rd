\name{svabu}
\alias{svabu}
\alias{svabu.fit}
\alias{zif}
\alias{is.present}
\alias{predictMCMC}
\title{
Binomial-Poisson and Binomial-ZIP models with single visit
}
\description{
Binomial-Poisson and Binomial-ZIP models with single visit
}
\usage{
svabu(formula, data, zeroinfl = TRUE, area = 1, N.max = NULL, inits, 
link.det = "logit", link.zif = "logit", 
model = TRUE, x = FALSE, ...)
svabu.fit(Y, X, Z, Q = NULL, zeroinfl = TRUE, area = 1, N.max = NULL, inits, 
link.det = "logit", link.zif = "logit", ...)
zif(x)
is.present(object, ...)
predictMCMC(object, ...)
}
\arguments{
  \item{formula}{
formula of the form \code{y ~ x | z}, where \code{y} is a vector of observations,
\code{x} is the set of covariates for the occurrence model,
\code{z} is the set of covariates for the detection model.
\code{x} can further expanded as \code{x1 + zif(x2)} into terms for
the nonzero count data part (\code{x1}) and the zero inflation
component (\code{zif(x2)}) using the \code{zif} special.
}
  \item{Y, X, Z, Q}{
vector of observation, design matrix for abundance model, design matrix for detection 
and design matrix for zero inflation model
}
  \item{data}{
data
}
  \item{area}{
area
}
  \item{N.max}{
maximum of true count values (for calculating the integral)
}
  \item{zeroinfl}{
logical, if the Binomial-ZIP model should be fitted
}
  \item{inits}{
initial values used by \code{link{optim}}
}
  \item{link.det, link.zif}{
link function for the detection and zero inflation parts of the model
}
  \item{model}{
a logical value indicating whether model frame should be included as a component of the returned value
}
  \item{x}{
logical values indicating whether the response vector and model matrix used in the 
fitting process should be returned as components of the returned value.
For the function \code{zif} it is any object to be returned.
}
  \item{object}{
a fitted object.
}
  \item{\dots}{
other arguments passed to the functions
}
}
\details{
ADD MORE STUFF HERE
}
\value{
An object of class 'svabu'.
}
\references{
Solymos et al.
}
\author{
Peter Solymos
}
\examples{
## give here examples
}
\keyword{ htest }
\keyword{ models }

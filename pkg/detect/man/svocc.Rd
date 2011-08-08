\name{svocc}
\alias{svocc}
\alias{svocc.fit}
\alias{extractMLE}
\title{
ZI Binomial model with single visit
}
\description{
ZI Binomial model with single visit
}
\usage{
svocc(formula, data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, 
method = c("optim", "dc"), inits, 
model = TRUE, x = FALSE, cl = NULL, ...)
svocc.fit(Y, X, Z, link.sta = "cloglog", link.det = "logit", penalized = FALSE, 
auc = FALSE, method = c("optim", "dc"), inits, cl = NULL, ...)
extractMLE(object, ...)
}
\arguments{
  \item{formula}{
formula of the form \code{y ~ x | z}, where \code{y} is a vector of observations,
\code{x} is the set of covariates for the occurrence model,
\code{z} is the set of covariates for the detection model
}
  \item{Y, X, Z}{
vector of observation, design matrix for occurrence model, and design matrix for detection model
}
  \item{data}{
data
}
  \item{link.sta, link.det}{
link function for the occurrence (true state) and detection model
}
  \item{penalized}{
logical, if penalized likelihood estimate should be computed
}
  \item{method}{
optimization or data cloning to be used as optimization
}
  \item{inits}{
initial values
}
  \item{model}{
a logical value indicating whether model frame should be included as a component of the returned value
}
  \item{x}{
logical values indicating whether the response vector and model matrix used in the 
fitting process should be returned as components of the returned value
}
  \item{cl}{
a cluster object for parallel MCMC calculations if method is data cloning
}
  \item{auc}{
logical, if AUC should be calculated
}
  \item{object}{
a fitted model object
}
  \item{\dots}{
other arguments passed to the functions
}
}
\details{
ADD MORE STUFF HERE
}
\value{
An object of class 'svocc'.
}
\references{
Lele et al.
Lele and Moreno PMLE
}
\author{
Peter Solymos and Monica Moreno
}
\examples{
## examples here
}
\keyword{ htest }
\keyword{ models }

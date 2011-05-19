\name{rsf}
\alias{rsf.fit}
\alias{rsf}
\alias{rspf}
\title{
Resource Selection (Probability) Functions
}
\description{
Resource selection (probability) functions for presnece-only wildlife data.
}
\usage{
rsf.fit(X, Y, m, link = "logit", B = 99, 
inits, method = "Nelder-Mead", control, ...)
rsf(formula, data, m, B = 99, inits, method = "Nelder-Mead", 
control, model = TRUE, x = FALSE, ...)
rspf(formula, data, m, B = 99, inits, method = "Nelder-Mead", 
control, model = TRUE, x = FALSE, ...)
}
\arguments{
  \item{formula}{
two sided model formula of the form \code{y ~ x}, 
where \code{y} is a vector of observations,
\code{x} is the set of covariates.
}
\item{data}{Y}{
vector of observations.
}
\item{data}{X}{
covariate matrix.
}
  \item{data}{
data.
}
  \item{m}{
number of matched points (matched design currently not implemented
in the package, contact the authors for more information)
}
  \item{B}{
number of bootstrap iterations to make.
}
  \item{inits}{
initial values, optional.
}
  \item{method}{
method to be used in \code{\link{optim}} for numerical optimization.
}
  \item{control}{
control options for \code{\link{optim}}.
}
  \item{model}{
a logical value indicating whether model frame should be included as a component of the returned value
}
  \item{x}{
logical values indicating whether the model matrix used in the 
fitting process should be returned as components of the returned value.
}
  \item{\dots}{
other arguments passed to the functions.
}
}
\details{
FIXME
}
\value{
An object of class \code{"rsf"} or \code{"rspf"}.
  \item{comp1 }{Description of 'comp1' FIXME}
  \item{comp2 }{Description of 'comp2' FIXME}
}
\references{
Lele, S.R. (2009) 
A new method for estimation of resource selection probability function. 
Journal of Wildlife Management 73, 122--127.

Lele, S. R. &  Keim, J. L. (2006) 
Weighted distributions and estimation of resource selection probability functions. 
Ecology 87, 3021--3028.
}
\author{
Subhash R. Lele, Jonah L. Keim, Peter Solymos
}
\examples{
## give an example
}
\keyword{ htest }

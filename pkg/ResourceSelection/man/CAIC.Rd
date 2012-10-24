\name{CAIC}
\alias{CAIC}
\alias{CAIC.default}
\title{
Consistent AIC
}
\description{
Consistent AIC
}
\usage{
CAIC(object, ..., alpha)
\method{CAIC}{default}(object, ..., alpha)
}
\arguments{
  \item{object}{
A fitted model object.
}
  \item{\dots}{
More fitted model objects.
}
  \item{alpha}{
Weight factor between 0 and 1 (see Details).
Default value is 0.5.
}
}
\details{
CAIC = alpha * AIC + (1 - alpha) * BIC
}
\value{
Atomic vector if only one input object provided,
a data frame similar to what is returned by
\code{\link{AIC}} and \code{\link{BIC}} 
if there are more than one input objects.
}
\references{
Bozdogan, H. 1987. 
Model selection and Akaike's information criterion (AIC): 
the general theory and its analytical extensions. 
Psychometrika, 52, 345-370.

Taper, M. 2004. Model identification from many candidates.
In: Taper, M. and Lele, S. R. (eds),
The Nature of Scientific Evidence: Statistical, Philosophical, 
and Empirical Considerations.
The University of Chicago Press, Chicago, IL, 567 pp.
}
\author{
Subhash Lele and Peter Solymos
}
\seealso{
\code{\link{AIC}}, \code{\link{BIC}} 
}
\examples{
## compare some random models
y <- rnorm(10)
a <- lm(y ~ runif(10))
b <- lm(y ~ runif(10))

0.5*(AIC(a) + BIC(a))
CAIC(a)
AIC(a)
CAIC(a, alpha=1)
BIC(a)
CAIC(a, alpha=0)

CAIC(a, b)
CAIC(a, b, alpha=0.2)

## you can use global option
## useful when inside of xv or bootstrap
## no need for extra argument
getOption("CAIC_alpha")
op <- options(CAIC_alpha = 0.2)
getOption("CAIC_alpha")
CAIC(a,b)
options(op)
getOption("CAIC_alpha")
}
\keyword{ htest }


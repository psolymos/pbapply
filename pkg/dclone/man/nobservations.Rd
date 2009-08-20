\encoding{UTF-8}
\name{nobservations}
\Rdversion{1.1}
\alias{nobservations}
\alias{nobservations.lm}
\title{
Number of Observations
}
\description{
Retrieves the number of observations from a fitted model object.
}
\usage{
nobservations(x, ...)
\method{nobservations}{lm}(x, ...)
}
\arguments{
  \item{x}{
A fitted object.
}
  \item{\dots}{
Other arguments to pass to the function.
}
}
\value{
Returns the number of observations.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link{BIC}} uses (based on) this function

The \code{\link[gdata]{nobs}} is equivalent function, gut has a
default method, which can be misleading in BIC calculations.
It is thus advisable to write class specific methods only.
}
\examples{
## example(glm)
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())
nobservations(glm.D93)
}
\keyword{ manip }

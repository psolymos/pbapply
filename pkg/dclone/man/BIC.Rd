\encoding{UTF-8}
\name{BIC}
\Rdversion{1.1}
\alias{BIC}
\alias{BIC.default}
\alias{extractBIC}
\alias{extractBIC.default}
\title{
BIC Calculation for Fitted Model Objects
}
\description{
Calculates Schwarz's Bayesian criterion (BIC) of fitted models.
}
\usage{
BIC(object, ...)
\method{BIC}{default}(object, ...)
extractBIC(fit, ...)
\method{extractBIC}{default}(fit, ...)
}
\arguments{
  \item{object, fit}{
Fitted model objects(s).
}
  \item{\dots}{
Optionally more fitted model objects.
}
}
\details{
The implementation relies entirely on \code{\link{AIC}} (consequently on \code{\link{logLik}} method)
 and \code{\link{nobservations}}.
If the number of observations cannot be retrieved from the fitted model, one should use
the function \code{\link{AIC}} by defining explicitly \eqn{k=log(n)}.
}
\value{
Statistic value similar to what returned by \code{\link{AIC}}.
}
\references{
Schwarz, G. E. 1978. Estimating the dimension of a model. \emph{Annals of Statistics}, 6: 461--464.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link{AIC}}
}
\examples{
## From example(glm)
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())
## these 2 should be equal
AIC(glm.D93, k=log(nobservations(glm.D93)))
BIC(glm.D93)
## using multiple arguments
glm.D93.2 <- glm(counts ~ outcome, family=poisson())
BIC(glm.D93, glm.D93.2)
}
\keyword{ htest }
\keyword{ models }

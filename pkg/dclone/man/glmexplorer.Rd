\encoding{UTF-8}
\name{glmexplorer}
\Rdversion{1.1}
\alias{glmexplorer}
\alias{summary.glmexplorer}
\alias{print.glmexplorer}
\alias{print.summary.glmexplorer}
\title{
Performs GLM for multiple responses.
}
\description{
Fits GLMs for multiple responses to quickly assess the results. 
Only for preliminary analyses.
}
\usage{
glmexplorer(formula, data, family=poisson(), select=FALSE, ...)
\method{print}{glmexplorer}(x, ...)
\method{summary}{glmexplorer}(object, crit = 0.05, ...)
\method{print}{summary.glmexplorer}(x, ...)
}
\arguments{
  \item{formula}{
A formula to be used. Left hand side is typically a matrix with
columns for each responses.
}
  \item{data}{
Data frame where to look up terms of the formula.
}
  \item{family}{
Family argument passed to GLM.
}
  \item{select}{
Logical, if \code{TRUE} the best model is picked up based on \code{\link{step}} and AIC values.
}
  \item{x, object}{
A fitted \code{glmexplorer} object.
}
  \item{crit}{
Critical value to use in the output to hide non significant (\eqn{p} >= \code{crit})
coefficients.
}
  \item{\dots}{
Further arguments passed to \code{\link{glm.fit}}.
}
}
\details{
Independent GLMs are fitted for each columns in the left hand side object of \code{formula}
by the \code{\link{glm.fit}} function.
}
\value{
The function returns a list of fitted GLM objects for each columns in the left hand
side object of \code{formula}.

The summary mathod returns the fitted coefficients in matrix format, 
and replaces values to \code{NA} where the respective \eqn{p} value 
is higher than or equel to \code{crit}.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link{glm.fit}}
}
\examples{
y <- matrix(rpois(40,2), 10, 4)
colnames(y) <- letters[1:4]
x <- data.frame(x1 = runif(10), x2 = as.factor(rep(0:1,5)))
mod <- glmexplorer(y ~ x1 + x2, x)
mod
summary(mod)
}
\keyword{ multivariate }
\keyword{ htest }
\keyword{ models }

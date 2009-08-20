\encoding{UTF-8}
\name{lincovtest}
\alias{lincovtest}
\alias{plot.lincovtest}
\title{Helper Function to Visualize Linearity in Logistic Regression}
\description{
The function help visualize if the relationship of a binary response and a continuous covariate is linar.
}
\usage{
lincovtest(y, x, probs = seq(0, 1, 0.25), na.rm = FALSE)
\method{plot}{lincovtest}(x, type = "b",
ylab = "Estimated coefficients", xlab = "Quantiles", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{vector, binary response to use in a binomial GLM.}
  \item{x}{vector, continuous covariate to use in a binomial GLM.}
  \item{probs}{sequence of quantiles to determine cut points for converting \code{x} into a factor.}
  \item{na.rm}{logical, should NA values removed.}
  \item{type, ylab, xlab, \dots}{argument passed to \code{\link{plot}} and \code{\link{par}} functions.}
}
\details{
The function makes a design variable based on quantiles of the covariate. Than performs a binomial GLM. The resulting coefficients for the levels of the design variable should indicate nonlinearity in the relationship. Coeffeicients relect effects on the logit scale (where we expect linearity), and effect sizes are compared to the first quantile (reference).
}
\value{
A named vector of class 'lincovtest'. It contains estimated coefficients for the levels of the design variable.

Plotting method makes a graph as a side effect.
}
\references{\url{http://www.abmi.ca/}}
\author{\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}}
\note{
This is not a really exact test, it just provides guide to judge linearity.
}
\seealso{\code{\link{glm}}, \code{\link{plot}}, \code{\link{par}}}
\examples{
## Positive linear relationship
y <- rep(0:1, each = 20)
x <- rep(1:40)
lct <- lincovtest(y, x)
## Plot it with some random results
opar <- par(mfrow = c(3, 3))
plot(lct, col = 2)
set.seed(123)
for (i in 1:8)
plot(lincovtest(y, rnorm(40)))
par(opar)
}
\keyword{ regression }

\name{predict.dcglm}
\alias{predict.dcglm}
\title{
Prediction for 'dcglm' Objects
}
\description{
Makes prediction for 'dcglm' objects, similar to \code{\link{predict.glm}}.
}
\usage{
\method{predict}{dcglm}(object, newdata = NULL, type = c("link", "response"), se = FALSE, ...)
}
\arguments{
  \item{object}{
a fitted object of class inheriting from 'dcglm'.
}
  \item{newdata}{
optionally, a data frame in which to look for variables with
which to predict.  If omitted, the fitted linear predictors
are used.
}
  \item{type}{
the type of prediction required.  The default is on the scale
of the linear predictors; the alternative \code{"response"} is on
the scale of the response variable.
}
  \item{se}{
logical switch indicating if standard errors are required.
}
  \item{\dots}{
additional arguments passed to \code{\link{jags.fit}}, such as \code{n.update},
\code{n.iter}, \code{n.chains} and \code{thin}.
}
}
\author{
Peter Solymos \email{solymos@ulaberta.ca}
}
\seealso{
\code{\link{predict.glm}}
}
\examples{
## see vignette
}
\keyword{ models }

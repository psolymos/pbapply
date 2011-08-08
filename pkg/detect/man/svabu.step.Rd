\name{svabu.step}
\alias{svabu.step}
\alias{svocc.step}
\title{
Automatic model selection
}
\description{
Automatic model selection
}
\usage{
svabu.step(object, model, trace = 1, steps = 1000, criter = c("AIC", "BIC"), test = FALSE, k = 2, control, ...)
svocc.step(object, model, trace = 1, steps = 1000, criter = c("AIC", "BIC", "cAUC"), test = FALSE, k = 2, control, ...)
}
\arguments{
  \item{object}{
a fitted model object}
  \item{model}{
true state or detection model
}
  \item{trace}{
info returned during the procedure
}
  \item{steps}{
max number of steps
}
  \item{criter}{
criterion to be minimized (cAUC=1-AUC)
}
  \item{test}{
lofical, if decrease in deviance should be tested
}
  \item{k}{
penalty to be used with AIC
}
  \item{control}{
controls for optimization, if missing taken from object
}
  \item{\dots}{
other arguments
}
}
\value{
A fitted model
}
\references{
???
}
\author{
Peter Solymos
}
\examples{
## examples here
}
\keyword{ htest }
\keyword{ models }

\encoding{UTF-8}
\name{AUC}
\Rdversion{1.1}
\alias{AUC}
\alias{AUC.singleocc}
\alias{rocplot}
\alias{rocplot.singleocc}
\alias{BIC.singleocc}
\title{
AUC BIC and ROC Plot for Fitted Models
}
\description{
Area under the reciever-operator (ROC) curve (AUC), ROC plot and Bayesian Information Criterion (BIC)
methods for fitted models.
}
\usage{
\method{BIC}{singleocc}(object, ...)
AUC(object, ...)
\method{AUC}{singleocc}(object, ...)
rocplot(x, ...)
\method{rocplot}{singleocc}(x, 
scales = seq(0, 1, 0.01), ylab, xlab, 
show = TRUE, add = FALSE, ...)
}
\arguments{
  \item{x, object}{
A fitted model object.
}
  \item{scales}{
Points et where ROC curve values should be calculated.
}
  \item{show}{
Logical, if a plot should be drawn.
}
  \item{add}{
Logical, if starte a new plot (\code{TRUE}) or draw onto an existing plot (\code{FALSE}).
}
  \item{ylab, xlab}{
Labels for axes. If not specified, defaolt labels are shown.
}
  \item{\dots}{
Optionally more fitted model objects, or additional graphical parameters for \code{rocplot}.
}
}
\value{
\code{BIC} returns BIC for a model, or a data frame with values for more model.

\code{AUC} returns AUC value for a model, or a data frame with values for more model.

\code{rocplot} returns the values used for the plot invisibly, and as a side effect it draws a graph.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos} and Monica Moreno
}
\examples{
data(rdata)
data(rdata)
x <- singleocc(y.obs ~ x1 | x1+x3, rdata)
AIC(x)
BIC(x)
AUC(x)
rocplot(x)
}
\keyword{ htest }
\keyword{ hplot }

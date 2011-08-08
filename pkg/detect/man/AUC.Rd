\name{AUC}
\alias{AUC}
\alias{rocplot}
\title{
AUC ROC plot for fitted models
}
\description{
Area under the reciever-operator (ROC) curve (AUC), and ROC plot methods for fitted models.
}
\usage{
AUC(object, ...)
rocplot(x, ...)
}
\arguments{
  \item{object, x}{
a fitted model object
}
  \item{\dots}{
other arguments
}
}
\value{
\code{AUC} returns AUC value for a model, or a data frame with values for more models.

\code{rocplot} returns the values used for the plot invisibly, and as a side effect it draws a graph.
}
\author{
Peter Solymos and Monica Moreno
}
\keyword{ hplot }
\keyword{ htest }

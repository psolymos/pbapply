\name{fancyPVAmodel}
\alias{fancyPVAmodel}
\title{
Print fancy model names in summaries
}
\description{
This function prints the fancy model names in summaries.
}
\usage{
fancyPVAmodel(object, initial = "PVA object:\n", part = 1:2)
}
\arguments{
  \item{object}{
A fitted 'pva' object.
}
  \item{initial}{
A fancy header for the fancy output.
}
  \item{part}{
Integer, \code{1} = model type is printed, \code{2} = data info is printed
\code{1:2} = both are printed.
}
}
\value{
Character with fancy model summary.
}
\author{
Khurram NAdeem and Peter Solymos
}
\keyword{ utils }

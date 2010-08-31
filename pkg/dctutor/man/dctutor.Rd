\name{dctutor}
\Rdversion{1.1}
\alias{dctutor}
\title{
Start a Tutorial
}
\description{
Start a tutorial (docs and demo)
}
\usage{
dctutor(tutorial = NULL, docs = TRUE, src = TRUE, demo = FALSE)
}
\arguments{
  \item{tutorial}{
A tutorial to start, shows available tutorial if \code{NULL}.
}
  \item{docs, src, demo}{
Logical, if the docs, source file and demo should be started.
}
}
\author{
Peter Solymos
}
\examples{
\dontrun{
dctutor()
dctutor("dclone")
}
}
\keyword{ IO }

\name{sourceDcExample}
\alias{sourceDcExample}
\alias{listDcExamples}
\title{
List, Source, Show and Run Data Cloning Examples
}
\description{
List, source, show and run data cloning examples.
}
\usage{
listDcExamples()
sourceDcExample(topic, envir = parent.frame())
}
\arguments{
  \item{topic}{
character, name of a data set.
}
  \item{envir}{
environment where the object \code{topic} should be assigned to.
It is the parent frame by default. No assignment is made when
\code{envir = NULL}.
}
}
\value{
\code{listDcExamples} lists the examples available
at \pkg{dclone} website at 
\url{http://dcr.r-forge.r-project.org/examples}
stored in \code{index.txt}.

\code{sourceDcExample} sources the example files
at \pkg{dclone} website at 
\url{http://dcr.r-forge.r-project.org/examples}.
Returns the object invisibly.

The default path to the examples directory is stored by the 
\code{getOption("dcmle.href")} and can be changed if required.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{"\linkS4class{dcmle}"}, \code{\link{dcmle}}
}
\examples{
\dontrun{
## index of currently available exampes
listDcExamples()

## source paramecium example
sourceDcExample("paramecium")
paramecium@model
custommodel(paramecium@model)
paramecium
}
}
\keyword{ utilities }
\keyword{ misc }

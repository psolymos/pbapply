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
sourceDcExample(topic, assign.global = TRUE)
}
\arguments{
  \item{topic}{
character, name of a data set.
}
  \item{assign.global}{
logical, if the object \code{topic} should be assigned to
the global environment.
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

The default path to the examples directory is stored by the 
\code{getOption("dcmle.href")} and can be changed if required.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{\link{dcmle-classes}}
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

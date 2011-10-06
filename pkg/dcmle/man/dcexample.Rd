\name{dcExample}
\alias{dcExample}
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
dcExample(topic, n.clones = NULL, ...)
}
\arguments{
  \item{topic}{
character, name of a data set.
}
  \item{n.clones}{
integer or \code{NULL}. If \code{NULL}, only the
data set structure is displayed. If not \code{NULL},
the model is fitted to the data by the \code{\link[dcmle]{dcmle}}
function.
}
  \item{assign.global}{
logical, if the object \code{topic} should be assigned to
the global environment.
}
  \item{\dots}{
additional arguments passed to \code{\link[dcmle]{dcmle}}.
}
}
\value{
\code{listDcExamples} lists the examples available
at \pkg{dclone} website at \url{http://dcr.r-forge.r-project.org/examples}
stored in \code{intex.txt}.

\code{sourceDcExample} sources the example files
at \pkg{dclone} website at \url{http://dcr.r-forge.r-project.org/examples}.

\code{dcExample} returns the fitted model object or 
\code{NULL} (if \code{n.clones = NULL}), 
prints summaries of data and fitted model 
(if \code{n.clones = NULL}) as a side effect.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{\link[dcmle]{dcmle}}
}
\examples{
\dontrun{
## index of currently available exampes
listDcExamples()

## source paramecium example
sourceDcExample("paramecioum")

## seeds example, structure only
dcExample("seeds")
## seeds example, structure and fit
dcExample("seeds", 2, n.iter=1000)
}
}
\keyword{ utilities }
\keyword{ misc }
\keyword{ documentation }

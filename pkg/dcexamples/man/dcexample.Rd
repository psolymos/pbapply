\name{dcexample}
\alias{dcexample}
\title{
Show and Run Data Cloning Examples
}
\description{
Show and run data cloning examples.
}
\usage{
dcexample(topic, n.clones = NULL, ...)
}
\arguments{
  \item{topic}{
name of a data set.
}
  \item{n.clones}{
integer or \code{NULL}. If \code{NULL}, only the
data set structure is displayed. If not \code{NULL},
the model is fitted to the data by the \code{\link[dcmle]{dcmle}}
function.
}
  \item{\dots}{
additional arguments passed to \code{\link[dcmle]{dcmle}}.
}
}
\value{
The function returns the fitted model object or 
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
help(package="dcexamples")

## seeds example, structure only
dcexample(rats)
## seeds example, structure and fit
dcexample(rats, 2, n.iter=1000)
}
}
\keyword{ utilities }
\keyword{ misc }
\keyword{ documentation }

\encoding{UTF-8}
\name{nclones}
\Rdversion{1.1}
\alias{nclones}
\alias{nclones.list}
\alias{nclones.default}
\title{
Number of Clones
}
\description{
Retrieves the number of clones from an object.
}
\usage{
nclones(x, ...)
\method{nclones}{default}(x, ...)
\method{nclones}{list}(x, ...)
}
\arguments{
  \item{x}{
An object.
}
  \item{\dots}{
Other arguments to be passed.
}
}
\value{
Returns the number of of clones, or \code{NULL}.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link{dclone}}
}
\examples{
x <- dclone(1:10, 10)
nclones(x)
}
\keyword{ manip }

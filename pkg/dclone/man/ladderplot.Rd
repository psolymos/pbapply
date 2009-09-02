\name{ladderplot}
\Rdversion{1.1}
\alias{ladderplot}
\title{
Ladder plot
}
\description{
Makes a ladder plot.
}
\usage{
ladderplot(x, col, pch=19, lty=1, vertical = TRUE, ...)
}
\arguments{
  \item{x}{
A matrix or data frame with at least 2 columns.
}
  \item{col}{
Color values to use for rows of \code{x}.
}
  \item{pch}{
Point type to use.
}
  \item{lty}{
Line type to use.
}
  \item{vertical}{
Logical, if the orientation of the ladderplot should be vertical or horizontal.
}
  \item{\dots}{
Other arguments passed to the function \code{\link{stripchart}}.
}
}
\details{
The function uses \code{\link{stripchart}} to plot 1-D scatter plots for each column in \code{x}.
Then points are joined by lines for each rows of \code{x}.
}
\value{
Makes a plot as a side effect.
Returns \code{NULL} invisibly.
}
\author{
Peter Solymos
}
\seealso{
\code{\link{lines}}, \code{\link{points}}, \code{\link{stripchart}}
}
\examples{
x<-data.frame(A=c(1:10), B=c(2:11)+rnorm(10))
y<-data.frame(x, C=c(1:10)+rnorm(10))
opar <- par(mfrow=c(1,3))
ladderplot(x)
ladderplot(x, col=1:10, vertical=FALSE)
ladderplot(y, col=1:10)
par(opar)
}
\keyword{ aplot }

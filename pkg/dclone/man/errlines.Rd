\name{errlines}
\Rdversion{1.1}
\alias{errlines}
\alias{errlines.default}
\title{
Plot error bars
}
\description{
The function plots error bars to existing plot.
}
\usage{
errlines(x, ...)
\method{errlines}{default}(x, y, type = "l", code = 0, 
width = 0, vertical = TRUE, ...)
}
\arguments{
  \item{x}{
Numeric vector with coordinates along the horizontal axis 
(if \code{vertical = FALSE}, this sets the vertical axis).
}
  \item{y}{
A matrix-like object with 2 columns for lower and upper values on the
vertical axis (if \code{vertical = FALSE}, this sets the horizontal axis).
}
  \item{type}{
Character, \code{"l"} for lines, \code{"b"} for boxes to be drawn.
}
  \item{code}{
Integer code, determining kind of ticks to be drawn. See Details.
}
  \item{width}{
Numeric, width of the ticks (if \code{type = "l"}) or 
width of the boxes (if \code{type = "b"}).
}
  \item{vertical}{
Logical, if errorbars should be plotted vertically or horizontally.
}
  \item{\dots}{
Other arguments passed to the function \code{\link{lines}}.
}
}
\details{
The \code{errlines} function uses \code{\link{lines}} to draw error bars
to existing plot.

If \code{code = 0} no ticks are drawn, if \code{code = 1}, only
lower ticks are drawn, if \code{code = 2} only
lower ticks are drawn, if \code{code = 3} both
lower and upper ticks are drawn.
}
\value{
Add error bars to an existing plot as a side effect.
Returns \code{NULL} invisibly.
}
\author{
Peter Solymos
}
\seealso{
\code{\link{lines}}, \code{\link{arrows}}, \code{\link{segments}}
}
\examples{
x <- 1:10
a <- rnorm(10,10)
a <- a[order(a)]
b <- runif(10)
y <- cbind(a-b, a+b+rev(b))
opar <- par(mfrow=c(2, 3))
plot(x, a, ylim = range(y))
errlines(x, y)
plot(x, a, ylim = range(y))
errlines(x, y, width = 0.5, code = 1)
plot(x, a, ylim = range(y))
errlines(x, y, width = 0.5, code = 3)
plot(x, a, ylim = range(y))
errlines(x, y, width = 0.5, code = 2, type = "b")
plot(x, a, ylim = range(y))
errlines(x, y, width = 0.5, code = 3, type = "b")
plot(x, a, ylim = range(y))
errlines(x, y, width = 0.5, code = 3, type = "b")
errlines(x, cbind(a-b/2, a+b/2+rev(b)/2))
par(opar)
}
\keyword{ aplot }

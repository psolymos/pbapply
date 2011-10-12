\name{sie}
\alias{sie-classes}
\alias{sie}
\alias{sieplot}
\title{
Small Island Effect (SIE) via Breakpoint Rregression
}
\description{
Fit a breakpoint regression model to data to find threshold for
the small island effect (SIE) as described in Lomolino...
}
\usage{
sie(S, A, method = "Nelder-Mead", ...)
sieplot(x, add = FALSE, ...)
}
\arguments{
  \item{S}{
untransformed species richness, vector.
}
  \item{A}{
untransformed area, vector.
}
  \item{x}{
a fitted model object of class 'sie'.
}
  \item{method}{
optimization mathod.
}
  \item{add}{
logcal, if lines should beadded to existing plot (\code{TRUE}),
or a new plot is to be drawn (\code{FALSE}, default).
}
  \item{\dots}{
graphical arguments passed to \code{\link{plot}}.
}
}
\details{
\code{sie} fits the breakpoint regression to the data,
richness is \code{log(S+0.5)} transformed, area is \code{log(A)}
transformed before analysis.
There is a \code{coef}, \code{summary}, \code{print} method for fitted objects.

\code{sieplot} plots the observed (transformed) data and the fitted line.
}
\value{
An S4 object of class 'sie' inheriting from class 'mle'.
}
\references{
Lomolino, M. V., and M. D. Weiser. 2001. 
Towards a more general species-area relationship: 
diversity on all islands, great and small. 
\emph{Journal of Biogeography}, 28, 431--445.
}
\author{
Peter Solymos
}
\examples{
data(sardata)
DAT <- sardata$islands[sardata$islands$study=="abbott1978plant",]
(x <- sie(DAT$S, DAT$A))
coef(x)
summary(x)
sieplot(x)
}
\keyword{ htest }

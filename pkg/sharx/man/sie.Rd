\name{sie}
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
sie(S, A)
sieplot(x, ...)
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
An object of class 'sie' inheriting from class 'lm'.
}
\references{
Lomolino
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

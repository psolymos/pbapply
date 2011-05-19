\name{kdepairs}
\alias{kdepairs}
\alias{kdepairs.default}
\alias{kdepairs.rsf}
\title{
Scatterplot Matrix with 2D Kernel Density
}
\description{
Scatterplot matrix with 2D kernel density.
}
\usage{
kdepairs(x, ...)
\method{kdepairs}{default}(x, n=25, density=TRUE, contour=TRUE, ...)
\method{kdepairs}{rsf}(x, n=25, density=TRUE, contour=TRUE, ...)
}
\arguments{
  \item{x}{
a matrix or data frame (or a fitted model object of class \code{"rsf"}
or \code{"rspf"}).
}
  \item{n}{
number of bins to be used in kernel density estimation.
}
  \item{density}{
logical, if shades corresponding to densities should be plotted.
}
  \item{contour}{
logical, if contour on top of shades should be plotted.
}
  \item{\dots}{
other possible arguments passed to \code{\link{pairs}}.
}
}
\value{
Produces a scatterplot matrix with histograms in diagonal, 2D kernel 
density estimates and contours in the lower half and bivariate scatterplots
with lowess smooth curves and Pearson correlation values 
in the upper half as a side effect.
Returns \code{NULL} invisibly.}
\author{
Peter Solymos
}
\seealso{
\code{\link{pairs}}, \code{\link{lowess}}, \code{\link[MASS]{kde2d}}, \code{\link{contour}}
}
\examples{
kdepairs(iris[1:4])
}
\keyword{ aplot }

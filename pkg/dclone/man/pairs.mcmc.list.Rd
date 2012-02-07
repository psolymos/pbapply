\name{pairs.mcmc.list}
\alias{pairs.mcmc.list}
\title{
Scatterplot Matrices for 'mcmc.list' Objects
}
\description{
A matrix of scatterplots is produced.
}
\usage{
\method{pairs}{mcmc.list}(x, n = 25, col = 1:length(x), 
    col.hist = "gold", col.image = terrain.colors(50), 
    density = TRUE, contour = TRUE, mean = TRUE, ...)
}
\arguments{
  \item{x}{
an 'mcmc.list' object.
}
  \item{n}{
number of of grid points in each direction for
two-dimensional kernel density estimation. 
Can be scalar or a length-2 integer vector.
}
  \item{col}{
color for chains in upper panel scatterplots.
}
  \item{col.hist}{
color for histogram fill in diagonal panels.
}
  \item{col.image}{
color palette for image plot in lower panel scatterplots.
}
  \item{density}{
logical, if image plot based on the two-dimensional kernel 
density estimation should be plotted in lower panel.
}
  \item{contour}{
logical, if contour plot based on the two-dimensional kernel 
density estimation should be plotted in lower panel.
}
  \item{mean}{
logical, if lines should indicate means of the
posterior densities in the panels.
}
  \item{\dots}{
additional graphical parameters/arguments.
}
}
\details{
The function produces a scatterplot matrix for 'mcmc.list' objects.
Diagonal panels are posterior densities with labels and
rug on the top. Upper panels are paiwise bivariate scatterplots
with coloring corresponding to chains, thus highlighting mixing properties
although not as clearly as trace plots. Lower panels
are two-dimensional kernel density estimates based on 
\code{\link[MASS]{kde2d}} function 
of \pkg{MASS} package using \code{\link{image}} 
and \code{\link{contour}}.
}
\value{
The function returns \code{NULL} invisibly and 
produces a plot as a side effect.
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link{pairs}}, \code{\link[coda]{plot.mcmc.list}}

Two-dimensional kernel density estimation: 
\code{\link[MASS]{kde2d}} in \pkg{MASS} package
}
\examples{
data(regmod)
pairs(regmod)
}
\keyword{ hplot }

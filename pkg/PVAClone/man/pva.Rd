\name{pva}
\alias{pva}
\title{
Population Viability Analysis
}
\description{
Populatio Viability Analysis (PVA).
}
\usage{
pva(x, model, n.clones, ...)
}
\arguments{
  \item{x}{
Numeric, a time series. Values must be non-negative,
missing values are allowed.
}
  \item{model}{
A 'pvamodel' object returned by a function, see Examples.
}
  \item{n.clones}{
Numeric, number of clones (possibly a vector).
If single value, it must be >1.
}
  \item{\dots}{
Arguments passed to underlying fitting functions,
most notably \code{n.update}, \code{n.iter},
\code{n.chains}, \code{thin}, \code{cl}.
See \code{\link[dcmle]{dcmle}}.
}
}
\details{
FIXME
}
\value{
An object of class 'pva', see \code{\link{pva-class}}.
}
\references{
FIXME
}
\author{
Khurram Nadeem and Peter Solymos
}
\seealso{
\code{\link{pva-class}}, \code{\link{pvamodel-class}}
}
\examples{
\dontrun{
data(songsparrow)
data(paurelia)

## Gompertz
m1 <- pva(songsparrow, "gompertz", c(5,10))
m2 <- pva(songsparrow, gompertz("poisson"), c(5,10))
m3 <- pva(songsparrow, gompertz("normal"), c(5,10))
m1na <- pva(paurelia, "gompertz", c(5,10))
m2na <- pva(paurelia, gompertz("poisson"), c(5,10))
m3na <- pva(paurelia, gompertz("normal"), c(5,10))
m1x <- pva(songsparrow, gompertz("normal"), 5)
m2x <- pva(songsparrow, gompertz("normal", fixed=c(tau=0.1)), 5)

## Ricker
m1 <- pva(songsparrow, "ricker", c(5,10))
m2 <- pva(songsparrow, ricker("poisson"), c(5,10))
m3 <- pva(songsparrow, ricker("normal"), c(5,10))
m1na <- pva(paurelia, "ricker", c(5,10))
m2na <- pva(paurelia, ricker("poisson"), c(5,10))
m3na <- pva(paurelia, ricker("normal"), c(5,10))
m1x <- pva(songsparrow, ricker("normal"), 5)
m2x <- pva(songsparrow, ricker("normal", fixed=c(tau=0.1)), 5)

m2x
coef(m2x)
vcov(m2x)
confint(m2x)
plot(as.mcmc.list(m2x))
plot(as.mcmc.list(m2x, backtransf=TRUE))
}
}
\keyword{ htest }
\keyword{ models }
\keyword{ ts }

\name{PVAClone-package}
\alias{PVAClone-package}
\alias{PVAClone}
\docType{package}
\title{
Population Viability Analysis with Data Cloning
}
\description{
Likelihood based population viability 
  analysis in the presence of observation error
  and missing data. The package fits,
  predicts, forecasts and compare various
  growth model types using data cloning.
}
\details{
FIXME
}
\author{
Khurram Nadeem, Peter Solymos

Maintainer: Peter Solymos <solymos@ualberta.ca>
}
\references{
FIXME
}
\keyword{ package }
\seealso{
\code{\link{pva}}
}
\examples{
data(songsparrow)
m1 <- pva(songsparrow, gompertz("none"), 2, n.iter=1000)
m2 <- pva(songsparrow, gompertz("poisson"), 2, n.iter=1000)
model.select(m1, m2)
}

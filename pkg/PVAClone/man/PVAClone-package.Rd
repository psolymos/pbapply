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
  predicts, forecasts and compares various
  growth model types using data cloning.
}
\details{
The package implements data cloning based population viability analysis 
methodology developed by Nadeem and Lele (2012). This includes, model estimation, model selection
and forecasting of future population abundances for estimate the extinction risk of a population of interest.
}
\author{
Khurram Nadeem, Peter Solymos

Maintainer: Peter Solymos <solymos@ualberta.ca>
}
\references{
Nadeem, K., Lele S. R., 2012.  Likelihood based population viability analysis in the presence of 
observation error. Oikos. doi: 10.1111/j.1600-0706.2011.20010.x
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

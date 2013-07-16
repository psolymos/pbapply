\name{PVAClone-package}
\alias{PVAClone-package}
\alias{PVAClone}
\alias{pvaclone}
\alias{PVA-package}
\alias{pva-package}
\alias{Pva-package}
\alias{PVA}
\alias{Pva}
\docType{package}
\title{
Population Viability Analysis with Data Cloning
}
\description{
Likelihood based population viability analysis in the 
  presence of observation error and missing data. 
  The package can be used to fit, compare, predict, 
  and forecast various growth model types using data cloning.
}
\details{
The package implements data cloning based population viability analysis 
methodology developed by Nadeem and Lele (2012). This includes
model estimation, model selection
and forecasting of future population abundances for estimate 
the extinction risk of a population of interest.

\code{\link{pva}}: main function for model fitting.

\code{\link{model.select}}: main function for model model selection.

Growth models: \code{\link{gompertz}}, \code{\link{ricker}},
\code{\link{bevertonholt}}, 
\code{\link{thetalogistic}}, \code{\link{thetalogistic_D}}.
}
\author{
Khurram Nadeem, Peter Solymos

Maintainer: Peter Solymos <solymos@ualberta.ca>
}
\references{
Nadeem, K., Lele S. R., 2012.  
Likelihood based population viability analysis in the presence of 
observation error. Oikos 121, 1656--1664.
}
\keyword{ package }
\seealso{
\code{\link{pva}}
}
\examples{
\dontrun{
## model selection for data with missing observations
data(songsparrow)
## model without observation error
m1 <- pva(songsparrow, gompertz("none"), 2, n.iter=1000)
## model with Poisson observation error
m2 <- pva(songsparrow, gompertz("poisson"), 2, n.iter=1000)
## model with Poisson observation error is strongly supported
model.select(m1, m2)
}
}

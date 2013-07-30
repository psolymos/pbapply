\name{detect-package}
\alias{detect-package}
\alias{detect}
\docType{package}
\title{
Analyzing Wildlife Data with Detection Error
}
\description{
  The package implements models to analyze
  site occupancy and count data models with detection error.
  The package development was supported by the 
  Alberta Biodiversity Monitoring Institute (\url{http://www.abmi.ca/}) 
  and the Boreal Avian Modelling (BAM) Project
  (\url{http://www.borealbirds.ca/}).
}
\details{
\code{\link{svocc}}: single visit occupancy model
(Lele et al. 2011, Moreno et al. 2010).

\code{\link{svabu}}: single visit abundance model
based on conditional maximum likelihood
(Solymos et al. 2012).

\code{\link{cmulti}}: conditional multinomial maximum likelihood 
estimation for removal and (point count) distance sampling,
efficient and flexible setup for varying methodologies
(Solymos et al. 2013).
}
\author{
Peter Solymos, Monica Moreno, Subhash R Lele

Maintainer: Peter Solymos <solymos@ualberta.ca>
}
\references{
Moreno, M. and Lele, S. R. 2010.
Improved estimation of site occupancy using penalized likelihood. 
\emph{Ecology} 91, 341--346.

Lele, S.R., Moreno, M. and Bayne, E. 2011. 
Dealing with detection error in site occupancy surveys: 
What can we do with a single survey? 
\emph{Journal of Plant Ecology} \bold{5(1)}, 22--31.

Solymos, P., Lele, S. R and Bayne, E. 2011.
Conditional likelihood approach for analyzing single visit
abundance survey data in the presence of zero inflation and
detection error. 
\emph{Environmetrics} \bold{23}, 197--205.

Solymos, P., Matsuoka, S. M., Bayne, E. M., Lele, S. R., Fontaine, P., 
Cumming, S. G., Stralberg, D., Schmiegelow, F. K. A. & Song, S. J., 2013.
Calibrating indices of avian density from non-standardized survey data: 
making the most of a messy situation. 
\emph{Methods in Ecology and Evolution}, in press.

Supporting info, including a tutorial for the above paper:
\url{http://dcr.r-forge.r-project.org/qpad/}
}
\keyword{ package }
\examples{
## FIXME!!! how to open tutorial/vignette?
}

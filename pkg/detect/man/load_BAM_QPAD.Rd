\name{load_BAM_QPAD}
\alias{load_BAM_QPAD}
\title{
Load BAM QPAD parameter estimates and support functions
}
\description{
Load BAM QPAD parameter estimates and support functions.
}
\usage{
load_BAM_QPAD(version)
}
\arguments{
  \item{version}{
version of the BAM QPAD estimates. List of selection is
provided if missing.
}
}
\value{
The function is called for its side effects: it sources the
BAM QPAD parameter estimates and support functions.
}
\references{
Solymos, P., Matsuoka, S. M., Bayne, E. M., Lele, S. R., Fontaine, P., 
Cumming, S. G., Stralberg, D., Schmiegelow, F. K. A. & Song, S. J., 2013.
Calibrating indices of avian density from non-standardized survey data: 
making the most of a messy situation. 
\emph{Methods in Ecology and Evolution}, \bold{4}, 1047--1058.

Supporting info, including a tutorial for the above paper:
\url{http://dcr.r-forge.r-project.org/qpad/}
}
\author{
Peter Solymos
}
\examples{
\dontrun{
load_BAM_QPAD(version = 1)
}
}
\keyword{ misc }

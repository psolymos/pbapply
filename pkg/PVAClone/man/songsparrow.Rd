\name{songsparrow}
\alias{songsparrow}
\docType{data}
\title{
Abundance time series of Song Sparrow
}
\description{
Counts for Song Sparrow (\emph{Melospiza melodia}) on
Mandarte Island, British Columbia, Canada from 1975--1998
reported in Saether et al. (2000).
}
\usage{data(songsparrow)}
\format{
  The format is:
 num [1:24] 35 31 45 48 66  9 19 26 54 ...
}
\details{
Song Sparrow abundance time series.
}
\source{
Peter Arcese kindly provided the Song Sparrow population
counts data.
}
\references{
Nadeem, K., Lele S. R., 2012.  
Likelihood based population viability analysis in the presence of 
observation error. Oikos 121, 1656--1664.

Saether, B. et al. 2000. 
Estimating the time to extinction in an island population 
of song sparrows. Proc. R. Soc. B 267, 621--626.

}
\examples{
data(songsparrow)
songsparrow
plot(songsparrow)
}
\keyword{datasets}

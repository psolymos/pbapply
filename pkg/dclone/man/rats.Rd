\name{rats}
\alias{rats}
\docType{data}
\title{
Rats data set
}
\description{
Rats data set from WinBUGS manual Vol. I. based on Gelfand et al. 1990.
}
\usage{data(rats)}
\format{
  The format is:
List of 4
 $ N: num 30
 $ T: num 5
 $ Y: num [1:30, 1:5] 151 145 147 155 135 159 141 159 177 134 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:30] "rat.1" "rat.2" "rat.3" "rat.4" ...
  .. ..$ : chr [1:5] "week.1" "week.2" "week.3" "week.4" ...
 $ x: Named num [1:5] 8 15 22 29 36
  ..- attr(*, "names")= chr [1:5] "week.1" "week.2" "week.3" "week.4" ...
}
\details{
Weights of 30 young rats in a control group were measured weekly for five weeks.
}
\source{
Obtained from JAGS example files from 
\url{http://sourceforge.net/projects/mcmc-jags/files/Examples/2.x/classic-bugs.tar.gz}
}
\references{
Gelfand, A. E., S. E. Hills, A. Racine-Poon, A. F. M. Smith, 1990.
Illustration of Bayesian Inference in Normal Data Models Using Gibbs Sampling.
\emph{Journal of the American Statistical Association} 
\strong{85}, 972--985.
}
\examples{
data(rats)
rats
}
\keyword{datasets}

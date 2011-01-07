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
## this is how the data set was created
rats <- list(N = 30, T = 5, 
    Y = structure(c(151, 145, 147, 155, 135, 159, 141, 159, 177, 134, 
    160, 143, 154, 171, 163, 160, 142, 156, 157, 152, 154, 139, 146, 
    157, 132, 160, 169, 157, 137, 153, 199, 199, 214, 200, 188, 210, 
    189, 201, 236, 182, 208, 188, 200, 221, 216, 207, 187, 203, 212, 
    203, 205, 190, 191, 211, 185, 207, 216, 205, 180, 200, 246, 249, 
    263, 237, 230, 252, 231, 248, 285, 220, 261, 220, 244, 270, 242, 
    248, 234, 243, 259, 246, 253, 225, 229, 250, 237, 257, 261, 248, 
    219, 244, 283, 293, 312, 272, 280, 298, 275, 297, 350, 260, 313, 
    273, 289, 326, 281, 288, 280, 283, 307, 286, 298, 267, 272, 285, 
    286, 303, 295, 289, 258, 286, 320, 354, 328, 297, 323, 331, 305, 
    338, 376, 296, 352, 314, 325, 358, 312, 324, 316, 317, 336, 321, 
    334, 302, 302, 323, 331, 345, 333, 316, 291, 324), .Dim = c(30, 5)),
    x = c(8.0, 15.0, 22.0, 29.0, 36.0))
rownames(rats$Y) <- paste("rat", 1:30, sep=".")
colnames(rats$Y) <- paste("week", 1:5, sep=".")
names(rats$x) <- paste("week", 1:5, sep=".")
}
\keyword{datasets}

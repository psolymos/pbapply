\name{simulateUsedAvail}
\alias{simulateUsedAvail}
\title{
Simulate Used-Aavailable Data
}
\description{
Simulates used-available data.
}
\usage{
simulateUsedAvail(data, parms, n.used, m, link="logit")
}
\arguments{
  \item{data}{
a matrix or data frame.
}
  \item{parms}{
coefficients corresponding to the columns of the design matrix
derived as \code{model.matrix(~., data)}.
}
  \item{n.used, m}{
number of used points (\code{n.used})
and number of available points for each (\code{m}).
}
  \item{link}{
character, the type of link function to be used.
}
}
\value{
A used-available data frame.
}
\author{
Subhash Lele, Peter Solymos
}
\examples{
n.used <- 1000
m <- 10
n <- n.used * m
set.seed(1234)
x <- data.frame(x1=rnorm(n), x2=runif(n))
cfs <- c(1.5,-1,0.5)
dat1 <- simulateUsedAvail(x, cfs, n.used, m, link="log")
str(dat1)
dat2 <- simulateUsedAvail(x, cfs, n.used, m, link="logit")
str(dat2)
}
\keyword{ manip }

\encoding{UTF-8}
\name{rdata}
\alias{rdata}
\docType{data}
\title{ Simulated Site-Occupancy Data With Detection Error }
\description{
  Simulated site-occupancy data with detection error.
}
\usage{data(rdata)}
\format{
  A data frame with 1000 observations on the 6 variables.
}
\details{
  For data generation, see Examples.
}
\examples{
## load and inspect the data set
data(rdata)
str(rdata)
## this script was used to generate the data set
set.seed(1234)
## number of sites
n <- 1000
## covariates
x1 <- runif(n, -1, 1)
x2 <- as.factor(rbinom(n, 1, 0.5))
x3 <- rnorm(n)
x4 <- rnorm(n)
## occurrence parameters
beta <- c(0.6, 0.5)
## detection parameters
theta <- c(0.4, -0.5, 0.3)
## expected values on the logit scale
mu <- model.matrix(~x1) \%*\% beta
nu <- model.matrix(~x1+x3) \%*\% theta
## occurrence and detection probabilities
p.occ <- exp(mu) / (1 + exp(mu))
p.det <- exp(nu) / (1 + exp(nu))
## true and observed site-occupancy status
y.true <- rbinom(n, 1, p.occ)
y.obs <- rbinom(n, 1, y.true * p.det)
## putting things together
rdata <- data.frame(y.obs, y.true, x1, x2, x3, x4)
}
\keyword{datasets}

\name{datocc}
\alias{datocc}
\docType{data}
\title{
Simulated example for occupancy model
}
\description{
Simulated example for occupancy model, see code below.
}
\usage{data(datocc)}
\format{
  A data frame with 1000 observations on the following 6 variables.
  \describe{
    \item{\code{Y}}{true occupancy}
    \item{\code{W}}{observations}
    \item{\code{x1}, \code{x2}, \code{x3}, \code{x4}}{
      random variables used as covariates}
    \item{\code{p.occ}}{probability of occurrence}
    \item{\code{p.det}}{probability of detection}
  }
}
\details{
This simulated example corresponds to the ZI Binomial model implemented in the function \code{\link{svocc}}.
}
\source{
Simulated example.
}
\references{
Lele, S.R., Moreno, M. and Bayne, E. (2011) 
Dealing with detection error in site occupancy surveys: 
What can we do with a single survey? 
\emph{Journal of Plant Ecology}, \bold{5(1)}, 22--31.
}
\examples{
data(datocc)
str(datocc)
\dontrun{
## simulation
n <- 1000
set.seed(1234)
x1 <- runif(n, -1, 1)
x2 <- as.factor(rbinom(n, 1, 0.5))
x3 <- rnorm(n)
x4 <- rnorm(n)
beta <- c(0.6, 0.5)
theta <- c(0.4, -0.5, 0.3)
X <- model.matrix(~ x1)
Z <- model.matrix(~ x1 + x3)
mu <- drop(X \%*\% beta)
nu <- drop(Z \%*\% theta)
p.occ <- binomial("cloglog")$linkinv(mu)
p.det <- binomial("logit")$linkinv(nu)
Y <- rbinom(n, 1, p.occ)
W <- rbinom(n, 1, Y * p.det)
datocc <- data.frame(Y, W, x1, x2, x3, x4, p.occ, p.det)
}
}
\keyword{datasets}

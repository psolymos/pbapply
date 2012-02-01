\name{databu}
\alias{databu}
\docType{data}
\title{
Simulated example for abundance model
}
\description{
Simulated example for abundance model, see code below.
}
\usage{data(databu)}
\format{
  A data frame with 1000 observations on the following 11 variables.
  \describe{
    \item{\code{N}}{true counts}
    \item{\code{Y}}{observed counts}
    \item{\code{x1, x2, x3, x4, x5, x6}}{random variables used as covariates}
    \item{\code{p}}{probability of detection}
    \item{\code{lambda}}{mean of the linear predictor}
    \item{\code{A}}{occupancy}
    \item{\code{phi}}{zero inflation probabilities}
  }
}
\details{
This simulated example corresponds to the Binomial - ZIP model implemented in the function \code{\link{svabu}}.
}
\source{
Simulated example.
}
\references{
Solymos, P., Lele, S. R and Bayne, E. 2011.
Conditional likelihood approach for analyzing single visit
abundance survey data in the presence of zero inflation and
detection error. 
\emph{Environmetrics} (in press)

Solymos, P., Lele, S. R and Bayne, E. 2011. Abundance estimation in the presence of zero
inflation and detection error using single visit data. Alberta Biodiversity Monitoring Institute, Alberta, Canada.
Technical Report No. ABMI-20061, August 24, 2011. Available at: 
\href{http://www.abmi.ca/FileDownloadServlet?filename=20061_ABMI_2011-08-24_Single_Visit_Abundance.pdf&dir=REPORTS_UPLOAD}{http://www.abmi.ca}
}
\examples{
data(databu)
str(databu)
\dontrun{
## simulation
n <- 1000
set.seed(1234)
x1 <- runif(n,0,1)
x2 <- rnorm(n,0,1)
x3 <- runif(n,-1,1)
x4 <- runif(n,-1,1)
x5 <- rbinom(n,1,0.6)
x6 <- rbinom(n,1,0.4)
x7 <- rnorm(n,0,1)
X <- model.matrix(~ x1 + x5)
Z <- model.matrix(~ x2 + x5)
Q <- model.matrix(~ x7)
beta <- c(2,-0.8,0.5)
theta <- c(1, 2, -0.5)
phi <- 0.3
p <- drop(binomial("logit")$linkinv(Z \%*\% theta))
lambda <- drop(exp(X \%*\% beta))
A <- rbinom(n, 1, 1-phi)
N <- rpois(n, lambda * A)
Y <- rbinom(n, N, p)
databu <- data.frame(N=N, Y=Y, x1, x2, x3, x4, x5, x6, p=p, lambda=lambda, A, phi)
}
}
\keyword{datasets}

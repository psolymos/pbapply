\encoding{UTF-8}
\name{regmod}
\Rdversion{1.1}
\alias{regmod}
\docType{data}
\title{
Exemplary MCMC list object
}
\description{
This data set was done via the code for \code{\link{jags.fit}} function.
}
\usage{data(regmod)}
\source{
See Example.
}
\examples{
data(regmod)
summary(regmod)
plot(regmod)
\dontrun{
## DATA GENERATION
## simple regression example from the JAGS manual
jfun <- function() {
    for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], tau)
        mu[i] <- alpha + beta * (x[i] - x.bar)
    }
    x.bar <- mean(x[])
    alpha ~ dnorm(0.0, 1.0E-4)
    beta ~ dnorm(0.0, 1.0E-4)
    sigma <- 1.0/sqrt(tau)
    tau ~ dgamma(1.0E-3, 1.0E-3)
}
## data generation
set.seed(1234)
N <- 100
alpha <- 1
beta <- -1
sigma <- 0.5
x <- runif(N)
linpred <- model.matrix(~x) \%*\% c(alpha, beta)
Y <- rnorm(N, mean = linpred, sd = sigma)
## list of data for the model
jdata <- list(N = N, Y = Y, x = x)
## what to monitor
jpara <- c("alpha", "beta", "sigma")
#### fit the model with JAGS
regmod <- jags.fit(jdata, jpara, jfun, n.chains = 3)
}
}
\keyword{datasets}

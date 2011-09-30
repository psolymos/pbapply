\name{parCodaSamples}
\alias{parCodaSamples}
\title{Generate posterior samples in mcmc.list format on parallel workers}
\description{
  This is function sets a trace
  monitor for all requested nodes, updates the model on each
  workers. Finally, it return the chains to the master and coerces the
  output to a single \code{mcmc.list} object.
}
\usage{
parCodaSamples(cl, model, variable.names, n.iter, thin = 1, ...)
}
\arguments{
  \item{cl}{a cluster object created by the \pkg{snow} package.}
  \item{model}{a jags model object}
  \item{variable.names}{a character vector giving the names of variables
    to be monitored}
  \item{n.iter}{number of iterations to monitor}
  \item{thin}{thinning interval for monitors}
  \item{...}{optional arguments that are passed to the update method
             for jags model objects}
}
\value{
  An \code{mcmc.list} object with possibly an \code{n.clones} attribute.
}
\author{Peter Solymos, \email{solymos@ualberta.ca}}
\seealso{\code{\link[rjags]{coda.samples}}, \code{\link{codaSamples}}}
\examples{
\dontrun{
model <- function() {
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
jdata <- list(N = N, Y = Y, x = x)
jpara <- c("alpha", "beta", "sigma")
## jags model on parallel workers
cl <- makeSOCKcluster(3)
parJagsModel(cl, name=res, file=model, data=jdata, 
    n.chains = 3, n.adapt=1000)
parUpdate(cl, res, n.iter=1000)
m <- parCodaSamples(cl, res, jpara, n.iter=2000)
## with data cloning
dcdata <- dclone(list(N = N, Y = Y, x = x), 2, multiply="N")
parJagsModel(cl, name=res2, file=model, data=dcdata, 
    n.chains = 3, n.adapt=1000)
parUpdate(cl, res2, n.iter=1000)
m2 <- parCodaSamples(cl, res2, jpara, n.iter=2000)
nclones(m2)
clusterEvalQ(cl, nclones(res2))
stopCluster(cl)
}
}
\keyword{models}

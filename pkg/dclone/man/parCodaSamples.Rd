\name{parCodaSamples}
\alias{parCodaSamples}
\title{Generate posterior samples in 'mcmc.list' 
format on parallel workers}
\description{
  This function sets a trace
  monitor for all requested nodes, updates the model on each
  workers. Finally, it return the chains to the master and coerces the
  output to a single \code{mcmc.list} object.
}
\usage{
parCodaSamples(cl, model, variable.names, n.iter, thin = 1, ...)
}
\arguments{
  \item{cl}{
    A cluster object created by \code{\link[snow]{makeCluster}}, or
    an integer. It can also be \code{NULL}, see 
    \code{\link{snowWrapper}}.
  }
  \item{model}{character, name of a jags model object}
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
\seealso{
Original sequential function in \pkg{rjags}: 
\code{\link[rjags]{coda.samples}}

Sequential \pkg{dclone}-ified version: \code{\link{codaSamples}}
}
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
## n.chains must be <= no. of workers
cl <- makePSOCKcluster(4)
parJagsModel(cl, name="res", file=model, data=jdata, 
    n.chains = 2, n.adapt=1000)
parUpdate(cl, "res", n.iter=1000)
m <- parCodaSamples(cl, "res", jpara, n.iter=2000)
stopifnot(2==nchain(m))
## with data cloning
dcdata <- dclone(list(N = N, Y = Y, x = x), 2, multiply="N")
parJagsModel(cl, name="res2", file=model, data=dcdata, 
    n.chains = 2, n.adapt=1000)
parUpdate(cl, "res2", n.iter=1000)
m2 <- parCodaSamples(cl, "res2", jpara, n.iter=2000)
stopifnot(2==nchain(m2))
nclones(m2)
## note: only 2 chanis were run on 2 workers!
clusterEvalQ(cl, if (exists("res2")) nclones(res2))
stopCluster(cl)
}
}
\keyword{models}

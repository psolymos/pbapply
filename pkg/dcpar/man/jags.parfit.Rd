\encoding{UTF-8}
\name{jags.parfit}
\Rdversion{1.1}
\alias{jags.parfit}
\alias{bugs.parfit}
\title{
Parallel computing with JAGS and WinBUGS/OpenBUGS
}
\description{
Do the same job as \code{\link[dclone]{jags.fit}} and \code{\link[dclone]{bugs.fit}}, 
but parallel chanis are run on parallel workers, thus more than 4 chains are enabled, and
computation can be faster for long MCMC runs.
}
\usage{
jags.parfit(cl, data, params, model, inits, n.chains = 3, ...)
bugs.parfit(cl, data, params, model, inits, n.chains = 3, 
bugs.seed = 1:n.chains, ...)
}
\arguments{
  \item{cl}{
A cluster object created by \code{\link[snow]{makeCluster}}.
}
  \item{data}{
A named list or environment containing the data. If an environment, data cloning might not work properly.
}
  \item{params}{
Character vector of parameters to be samples.
}
  \item{model}{
Character string (name of the model file) or a function containing the model (see Examples).
}
  \item{inits}{
Specification of initial values in the form of a list or a function. 
For the function \code{jags.parfit}, \code{inits} must contain RNG seed information, too (see Initialization at \code{\link[rjags]{jags.model}}).
For the function \code{bugs.parfit}, \code{inits} has to contain only initial values without random seed specifications 
(but see argument \code{bugs.seed}).
}
  \item{n.chains}{
Number of chains to generate, must be higher than 1. Ideally, this is equal to the number of parallel workers in the cluster.
}
  \item{bugs.seed}{
Random seeds for each chains (can be a vector or a list containing integers that are different from each other).
}
  \item{...}{
Other arguments accepted by \code{\link[dclone]{jags.fit}} and \code{\link[dclone]{bugs.fit}}.
}
}
\details{
Chains are run on parallel workers, and the results are combined in the end.
}
\value{
An \code{mcmc.list} object.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link[dclone]{jags.fit}}, \code{\link[dclone]{bugs.fit}}
}
\examples{
\dontrun{
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
initsfun <- function(i) list(alpha=rnorm(1), "beta"=rnorm(1), "tau"=rlnorm(1),
    ".RNG.name"="base::Wichmann-Hill", ".RNG.seed"=rpois(1, 1000))
inits <- lapply(1:3, initsfun)
## fit the model without parallelization
system.time(regmod <- jags.fit(jdata, jpara, jfun, inits, n.chains = 3))
## fit the model with parallelization
cl <- makeCluster(3, type = "SOCK")
system.time(pregmod <- jags.parfit(cl, jdata, jpara, jfun, inits, n.chains = 3))
stopCluster(cl)
}
}
\keyword{ models }

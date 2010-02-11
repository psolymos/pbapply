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
set.seed(1234)
n <- 20
x <- runif(n, -1, 1)
X <- model.matrix(~x)
beta <- c(2, -1)
mu <- X \%*\% beta
Y <- rpois(n, exp(mu))
glm.model <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- inprod(X[i,], beta[1,])
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
}
dat <- list(Y=Y, X=X, n=n, np=ncol(X))
m <- jags.fit(dat, "beta", glm.model)
cl <- makeCluster(3, type = "SOCK")
pm <- jags.parfit(cl, dat, "beta", glm.model)
## chains are not identical -- this is good
pm[1:2,]
summary(pm)
stopCluster(cl)
}
}
\keyword{ models }

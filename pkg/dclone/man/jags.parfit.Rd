\encoding{UTF-8}
\name{jags.parfit}
\Rdversion{1.1}
\alias{jags.parfit}
\title{
Parallel computing with JAGS
}
\description{
Do the same job as \code{\link[dclone]{jags.fit}}, 
but parallel chanis are run on parallel workers, thus 
computations can be faster (up to 1/\code{n.chains}) for long MCMC runs.
}
\usage{
jags.parfit(cl, data, params, model, inits = NULL, n.chains = 3, ...)
}
\arguments{
  \item{cl}{
A cluster object created by \code{\link[snow]{makeCluster}}.
}
  \item{data}{
A named list or environment containing the data. If an environment, data cloning might not work properly (has to be done manually).
}
  \item{params}{
Character vector of parameters to be sampled.
}
  \item{model}{
Character string (name of the model file), a function containing the model, or a or \code{\link{custommodel}} object (see Examples).
}
  \item{inits}{
Specification of initial values in the form of a list or a function, can be missing if \code{n.chains <= 4}. 
\code{inits} must contain RNG seed information, too (see Initialization at \code{\link[rjags]{jags.model}}).
}
  \item{n.chains}{
Number of chains to generate, must be higher than 1. Ideally, this is equal to the number of parallel workers in the cluster.
}
  \item{...}{
Other arguments passed to \code{\link[dclone]{jags.fit}}.
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
\code{\link[dclone]{jags.fit}}
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

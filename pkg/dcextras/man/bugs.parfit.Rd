\encoding{UTF-8}
\name{bugs.parfit}
\Rdversion{1.1}
\alias{bugs.parfit}
\title{
Parallel computing with JAGS
}
\description{
Do the same job as \code{\link[dclone]{jags.fit}}, 
but parallel chanis are run on parallel workers, thus 
computations can be faster (up to 1/\code{n.chains}) for long MCMC runs.
}
\usage{
bugs.parfit(cl, data, params, model, inits = NULL, n.chains = 3, 
seed, program=c("winbugs", "openbugs"), ...)
}
\arguments{
  \item{cl}{
A cluster object created by \code{\link[snow]{makeCluster}}.
}
  \item{data}{
A named list or environment containing the data. If an environment, it is coerced to a list.
}
  \item{params}{
Character vector of parameters to be sampled.
}
  \item{model}{
Character string (name of the model file), a function containing the model, or a or \code{\link{custommodel}} object (see Examples).
}
  \item{inits}{
Optional specification of initial values in the form of a list or a function. 
If \code{NULL}, initial values will be generated automatically.
If this is a function, it must be self containing, i.e. not having references to R objects outside of the
function, or the objects should be exported with \code{\link{clusterExport}} before calling \code{bugs.parfit}.
}
  \item{n.chains}{
Number of chains to generate, must be higher than 1. Ideally, this is equal to the number of parallel workers in the cluster.
}
  \item{program}{
The program to use, not case sensitive.
\code{winbugs} calls the function \code{\link[R2WinBUGS]{bugs}},
\code{openbugs} calls the function \code{\link[R2WinBUGS]{openbugs}} and requires the CRAN package \pkg{BRugs}.
}
  \item{seed}{
Random seed (\code{bugs.seed} argument for \code{\link[R2WinBUGS]{bugs}}, \code{seed} argument for \code{\link[R2WinBUGS]{openbugs}}).
Length must equal \code{n.chains}.
}
  \item{...}{
Other arguments passed to \code{\link[dclone]{bugs.fit}}.
}
}
\details{
Chains are run on parallel workers, and the results are combined in the end.
}
\value{
An \code{mcmc.list} object, Cannot produce output in \code{bugs} object class format.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link[dclone]{bugs.fit}}
}
\examples{
\dontrun{
## fitting with WinBUGS, bugs example
data(schools)
dat <- list(J = nrow(schools), y = schools$estimate, sigma.y = schools$sd)
bugs.model <- function(){
       for (j in 1:J){
         y[j] ~ dnorm (theta[j], tau.y[j])
         theta[j] ~ dnorm (mu.theta, tau.theta)
         tau.y[j] <- pow(sigma.y[j], -2)
       }
       mu.theta ~ dnorm (0.0, 1.0E-6)
       tau.theta <- pow(sigma.theta, -2)
       sigma.theta ~ dunif (0, 1000)
     }  
inits <- NULL
param <- c("mu.theta", "sigma.theta")
cl <- makePSOCKcluster(3)
sim <- bugs.parfit(cl, dat, param, bugs.model, inits, seed=1:3)
sim2 <- bugs.parfit(cl, dat, param, bugs.model, inits, program="openbugs",
    n.thin=1, seed=1:3)
## examples on how to use initial values
## self contained function
inits <- function() list(mu.theta=rnorm(1), sigma.theta=rlnorm(1))
sim <- bugs.parfit(cl, dat, param, bugs.model, inits, seed=1:3)
sim2 <- bugs.parfit(cl, dat, param, bugs.model, inits, program="openbugs",
    n.thin=1, seed=1:3)
## function pointing to the global environment
fun <- function() list(mu.theta=rnorm(1), sigma.theta=rlnorm(1))
inits <- function() fun()
clusterExport(cl, "fun")
sim <- bugs.parfit(cl, dat, param, bugs.model, inits, seed=1:3)
sim2 <- bugs.parfit(cl, dat, param, bugs.model, inits, program="openbugs",
    n.thin=1, seed=1:3)
stopCluster(cl)
}
}
\keyword{ models }

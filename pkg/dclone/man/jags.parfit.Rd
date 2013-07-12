\encoding{UTF-8}
\name{jags.parfit}
\Rdversion{1.1}
\alias{jags.parfit}
\title{
Parallel computing with JAGS
}
\description{
Does the same job as \code{\link[dclone]{jags.fit}}, 
but parallel chanis are run on parallel workers, thus 
computations can be faster (up to 1/\code{n.chains}) for long MCMC runs.
}
\usage{
jags.parfit(cl, data, params, model, inits = NULL, n.chains = 3, ...)
}
\arguments{
  \item{cl}{
A cluster object created by \code{\link[snow]{makeCluster}}, or
an integer, see \code{\link{parDosa}} and 
\code{\link{evalParallelArgument}}.
}
  \item{data}{
A named list or environment containing the data. If an environment, 
\code{data} is coerced into a list.
}
  \item{params}{
Character vector of parameters to be sampled.
}
  \item{model}{
Character string (name of the model file), a function 
containing the model, or a or \code{\link{custommodel}} 
object (see Examples).
}
  \item{inits}{
Specification of initial values in the form of a 
list or a function, can be missing.
Missing value setting can include RNG seed information,
see Initialization at \code{\link[rjags]{jags.model}}.
If this is a function and using 'snow' type
cluster as \code{cl}, the function must be self containing, i.e. 
not having references to R objects outside of the
function, or the objects should be exported with 
\code{\link{clusterExport}} before calling \code{jags.parfit}.
Forking type parallelism does not require such attention.
}
  \item{n.chains}{
Number of chains to generate, must be higher than 1. Ideally, 
this is equal to the number of parallel workers in the cluster.
}
  \item{...}{
Other arguments passed to \code{\link[dclone]{jags.fit}}.
}
}
\details{
Chains are run on parallel workers, 
and the results are combined in the end.

No update method is available for parallel \code{mcmc.list} objects.
See \code{\link{parUpdate}} and related parallel functions
(\code{\link{parJagsModel}}, \code{\link{parCodaSamples}}) 
for such purpose.

Additionally loaded JAGS modules (e.g. \code{"glm"}, 
\code{"lecuyer"}) need to be loaded to the workers
when using 'snow' type cluster as \code{cl} argument. See Examples.

The use of the \code{"lecuyer"} module is recommended when
running more than 4 chains. See Examples and 
\code{\link{parallel.inits}}.
}
\value{
An \code{mcmc.list} object.
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{
Sequential version: \code{\link[dclone]{jags.fit}}

Function for stepwise modeling with JAGS: \code{\link{parJagsModel}}, 
\code{\link{parUpdate}}, \code{\link{parCodaSamples}}
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
load.module("glm")
m <- jags.fit(dat, "beta", glm.model)
cl <- makeCluster(3, type = "SOCK")
## load glm module
tmp <- clusterEvalQ(cl, library(dclone))
parLoadModule(cl, "glm")
pm <- jags.parfit(cl, dat, "beta", glm.model)
## chains are not identical -- this is good
pm[1:2,]
summary(pm)
## examples on how to use initial values
## fixed initial values
inits <- list(list(beta=matrix(c(0,1),1,2)), 
    list(beta=matrix(c(1,0),1,2)), 
    list(beta=matrix(c(0,0),1,2)))
pm2 <- jags.parfit(cl, dat, "beta", glm.model, inits)
## random numbers generated prior to jags.parfit
inits <- list(list(beta=matrix(rnorm(2),1,2)), 
    list(beta=matrix(rnorm(2),1,2)), 
    list(beta=matrix(rnorm(2),1,2)))
pm3 <- jags.parfit(cl, dat, "beta", glm.model, inits)
## self contained function
inits <- function() list(beta=matrix(rnorm(2),1,2))
pm4 <- jags.parfit(cl, dat, "beta", glm.model, inits)
## function pointing to the global environment
fun <- function() list(beta=matrix(rnorm(2),1,2))
inits <- function() fun()
clusterExport(cl, "fun")
## using the L'Ecuyer module with 6 chains
load.module("lecuyer")
parLoadModule(cl,"lecuyer")
pm5 <- jags.parfit(cl, dat, "beta", glm.model, inits,
    n.chains=6)
nchain(pm5)
unload.module("lecuyer")
parUnloadModule(cl,"lecuyer")
stopCluster(cl)
## multicore type forking
if (.Platform$OS.type != "windows") {
pm6 <- jags.parfit(3, dat, "beta", glm.model)
}
}
}
\keyword{ models }

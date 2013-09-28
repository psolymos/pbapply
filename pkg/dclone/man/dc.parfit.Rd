\encoding{UTF-8}
\name{dc.parfit}
\Rdversion{1.1}
\alias{dc.parfit}
\title{
Parallel model fitting with data cloning
}
\description{
Iterative model fitting on parallel workers with different 
numbers of clones.
}
\usage{
dc.parfit(cl, data, params, model, inits, n.clones, 
    multiply=NULL, unchanged=NULL, 
    update = NULL, updatefun = NULL, initsfun = NULL, 
    flavour = c("jags", "bugs"), n.chains = 3,
    partype=c("balancing", "parchains", "both"), ...)
}
\arguments{
  \item{cl}{
A cluster object created by \code{\link[parallel]{makeCluster}}, or
an integer, see \code{\link{parDosa}} and 
\code{\link{evalParallelArgument}}.
}
  \item{data}{
A named list (or environment) containing the data.
}
  \item{params}{
Character vector of parameters to be sampled.
It can be a list of 2 vectors, 1st element
is used as parameters to monitor, the 2nd is used
as parameters to use in calculating the data cloning
diagnostics. (\code{partype = "both"}
currently cannot handle \code{params} as list.)
}
  \item{model}{
Character string (name of the model file), a function containing the 
model, or a or \code{\link{custommodel}} object (see Examples).
}
  \item{inits}{
Optional specification of initial values in the form of a list 
or a function (see Initialization at \code{\link[rjags]{jags.model}}). 
If missing, will be treated as \code{NULL} and initial values 
will be generated automatically.
If this is a function, it must be self containing, i.e. not 
having references to R objects outside of the
function, or the objects should be exported with 
\code{\link{clusterExport}} before calling \code{dc.parfit}.
}
  \item{n.clones}{
An integer vector containing the numbers of clones to use iteratively.
}
  \item{multiply}{
Numeric or character index for list element(s) in the \code{data} 
argument to be multiplied by the number of clones instead of repetitions.
}
  \item{unchanged}{
Numeric or character index for list element(s) in the \code{data} 
argument to be left unchanged.
}
  \item{update}{
Numeric or character index for list element(s) in the \code{data} 
argument that has to be updated by \code{updatefun} in each iterations. 
This usually is for making priors more informative, and enhancing 
convergence.
This argument is ignored if size balancing is used (default), and not
ignored when multiple parallel chains are used.
}
  \item{updatefun}{
A function to use for updating \code{data[[update]]}. 
It should take an 'mcmc.list' object as 1st argument,
2nd argument can be the number of clones.
This argument is ignored if size balancing is used (default), and not
ignored when multiple parallel chains are used.
}
  \item{initsfun}{
A function to use for generating initial values, \code{inits} 
are updated by the object returned by this function from the 
second iteration. If initial values
are not dependent on the previous iteration, this should be \code{NULL}, 
otherwise, it should take an 'mcmc.list' object as 1st argument, 
2nd argument can be the number of clones. This feature is useful if 
latent nodes are provided in \code{inits}
so it also requires to be cloned for subsequent iterations.
The 1st argument of the \code{initsfun} function is ignored if 
\code{partype != "parchains"} but the function must have a first 
argument regardless, 
see Examples.
}
  \item{flavour}{
  If \code{"jags"}, the function \code{\link{jags.fit}} is called. 
  If \code{"bugs"}, the function \code{\link{bugs.fit}} is called
  (available with \code{partype = "balancing"} only).
  See Details.
}
  \item{partype}{
Type of parallel workload distribution, see Details. 
}
  \item{n.chains}{
Number of chains to generate.
}
  \item{\dots}{
  Other values supplied to \code{\link[dclone]{jags.fit}}, or 
  \code{\link[dclone]{bugs.fit}}, depending on the \code{flavour} 
  argument.
}
}
\details{
The \code{dc.parfit} is a parallel computing version of 
\code{\link[dclone]{dc.fit}}.
After parallel computations, temporary objects passed to workers and
the \pkg{dclone} package is cleaned up. It is not guaranteed
that objects already on the workers and independently loaded packages
are not affected. Best to start new instances beforehand.

\code{partype="balancing"} distributes each model corresponding
to values in \code{n.clones} as jobs to workers according to size 
balancing (see \code{\link{snowWrapper}}). \code{partype="parchains"} 
makes repeated calls
to \code{\link{jags.parfit}} for each value in \code{n.clones}.
\code{partype="both"} also calls \code{\link{jags.parfit}} but
each chain of each cloned model is distributed as separate job to the 
workers.

The vector \code{n.clones} is used to determine size balancing. 
If load balancing is also desired 
besides of size balancing (e.g. due to unequal performance of 
the workers, the option \code{"dclone.LB"} should be set to \code{TRUE} 
(by using \code{options("dclone.LB" = TRUE)}).
 By default, the \code{"dclone.LB"}
option is \code{FALSE} for reproducibility reasons.

Some arguments from \code{\link{dc.fit}} are not available in parallel 
version (\code{update}, \code{updatefun}, \code{initsfun}) 
when size balancing is used 
(\code{partype} is \code{"balancing"} or \code{"both"}).
These arguments are evaluated only when \code{partype="parchains"}.

Size balancing is recommended if \code{n.clones} is a relatively long 
vector, while parallel chains might be more efficient when 
\code{n.clones} has few elements.
For efficiency reasons, a combination of the two 
(\code{partype="both"}) is preferred if cluster size allows it.

Only \code{partype="balancing"} is available for \code{flavour="bugs"}.

Additionally loaded JAGS modules (e.g. \code{"glm"}) need to be loaded
to the workers.
}
\value{
An object inheriting from the class 'mcmc.list'.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.

Lele, S. R., K. Nadeem and B. Schmuland, 2010.
Estimability and likelihood inference for generalized 
linear mixed models using data cloning.
\emph{Journal of the American Statistical Association}
\strong{105}, 1617--1625.

Solymos, P., 2010. dclone: Data Cloning in R.
\emph{The R Journal} \strong{2(2)}, 29--37.
URL: \url{http://journal.r-project.org/archive/2010-2/RJournal_2010-2_Solymos.pdf}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{
Sequential version: \code{\link[dclone]{dc.fit}}.

Optimizing the number of workers: \code{\link{clusterSize}}, 
\code{\link{plotClusterSize}}.

Underlying functions: \code{\link[dclone]{jags.fit}},
\code{\link[dclone]{bugs.fit}}.
}
\examples{
\dontrun{
set.seed(1234)
n <- 20
x <- runif(n, -1, 1)
X <- model.matrix(~x)
beta <- c(2, -1)
mu <- crossprod(t(X), beta)
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
k <- 1:3
## sequential version
dcm <- dc.fit(dat, "beta", glm.model, n.clones=k, multiply="n", 
    unchanged="np")
## parallel version with snow
cl <- makePSOCKcluster(3)
pdcm1 <- dc.parfit(cl, dat, "beta", glm.model, n.clones=k, 
    multiply="n", unchanged="np",
    partype="balancing")
pdcm2 <- dc.parfit(cl, dat, "beta", glm.model, n.clones=k, 
    multiply="n", unchanged="np",
    partype="parchains")
pdcm3 <- dc.parfit(cl, dat, "beta", glm.model, n.clones=k, 
    multiply="n", unchanged="np",
    partype="both")
summary(dcm)
summary(pdcm1)
summary(pdcm2)
summary(pdcm3)
stopCluster(cl)
## multicore type forking
if (.Platform$OS.type != "windows") {
mcdcm1 <- dc.parfit(3, dat, "beta", glm.model, n.clones=k, 
    multiply="n", unchanged="np",
    partype="balancing")
mcdcm2 <- dc.parfit(3, dat, "beta", glm.model, n.clones=k, 
    multiply="n", unchanged="np",
    partype="parchains")
mcdcm3 <- dc.parfit(3, dat, "beta", glm.model, n.clones=k, 
    multiply="n", unchanged="np",
    partype="both")
}

## Using WinBUGS/OpenBUGS
library(R2WinBUGS)
data(schools)
dat <- list(J = nrow(schools), y = schools$estimate, 
    sigma.y = schools$sd)
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
inits <- function(){
    list(theta=rnorm(nrow(schools), 0, 100), mu.theta=rnorm(1, 0, 100),
         sigma.theta=runif(1, 0, 100))
}
param <- c("mu.theta", "sigma.theta")
cl <- makePSOCKcluster(2)
if (.Platform$OS.type == "windows") {
sim2 <- dc.parfit(cl, dat, param, bugs.model, n.clones=1:2, 
    flavour="bugs", program="WinBUGS", multiply="J",
    n.iter=2000, n.thin=1)
summary(sim2)
}
sim3 <- dc.parfit(cl, dat, param, bugs.model, n.clones=1:2, 
    flavour="bugs", program="brugs", multiply="J",
    n.iter=2000, n.thin=1)
summary(sim3)
library(R2OpenBUGS)
sim4 <- dc.parfit(cl, dat, param, bugs.model, n.clones=1:2, 
    flavour="bugs", program="openbugs", multiply="J",
    n.iter=2000, n.thin=1)
summary(sim4)
stopCluster(cl)

## simulation for Poisson GLMM with inits
set.seed(1234)
n <- 5
beta <- c(2, -1)
sigma <- 0.1
alpha <- rnorm(n, 0, sigma)
x <- runif(n)
X <- model.matrix(~x)
linpred <- crossprod(t(X), beta) + alpha
Y <- rpois(n, exp(linpred))
## JAGS model as a function
jfun1 <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta)
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:np) {
        beta[j] ~ dnorm(0, 0.001)
    }
    sigma ~ dlnorm(0, 0.001)
}
## data
jdata <- list(n = n, Y = Y, X = X, np = NCOL(X))
## inits with latent variable and parameters
ini <- list(alpha=rep(0,n), beta=rep(0, NCOL(X)))
## model arg is necessary as 1st arg,
## but not used when partype!=parchains
ifun <-
function(model, n.clones) {
    list(alpha=dclone(rep(0,n), n.clones),
        beta=c(0,0))
}
## make cluster
cl <- makePSOCKcluster(2)
## pass global n variable used in ifun to workers
tmp <- clusterExport(cl, "n")
## fit the model
jmod2 <- dc.parfit(cl, jdata, c("beta", "sigma"), jfun1, ini,
    n.clones = 1:2, multiply = "n", unchanged = "np",
    initsfun=ifun, partype="balancing")
stopCluster(cl)
}
}
\keyword{ models }
\keyword{ htest }

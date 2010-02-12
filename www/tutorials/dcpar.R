## We learn now to run MCMC chains in parallel, how to evaluate
## simulations or bootstrap iteration in parallel, how to do data cloning
## with different number of clones in parallel.

## Same prerequisites as for Lecture 1, plus the snow R package
## installed. I will distribute copies of the dcpar package (very
## developmental).

## Topics to cover:
## - parallel simulations/bootstrap with the snow package
## - parallel MCMC chain computations, issues with random numbers
## - using snow functions inside other functions: difficulties and solutions
## - parallel data cloning with different number of clones: size balancing 

## recommended reading: Simple Parallel Statistical Computing in R
## http://www.bepress.com/uwbiostat/paper193/

## by loading dcpar, we get dclone and snow as well
library(dcpar)

## 1. the snow package (plus few dcdim functions)

## snow = Simple Network Of Workstations

## virtual connection between processes
##    * Socket
##    * PVM (Parallel Virtual Machine)
##    * MPI (Message Passing Interface)

## for multi-core machines, we use SOCK

## start a cluster
cl <- makeCluster(2, type = "SOCK")
cl
## end a cluster
stopCluster(cl)

## now start a cluster
cl <- makeCluster(2, type = "SOCK")

## evaluate a function with its arguments on each worker
## snow::clusterCall(cl, fun, ...)
clusterCall(cl, runif, 5)
## same numbers on each worker, but random sequences
## dcpar::clusterSeed
## different seed, different results
clusterSeed(cl, 1:2)
clusterCall(cl, runif, 5)
## same seed, identical results
clusterSeed(cl, 1)
clusterCall(cl, runif, 5)

## evaluate a literal expression on each worker
## snow::clusterEvalQ(cl, expr)
clusterEvalQ(cl, library(dcpar))

## splits the problem (seq) into length(cl) quasi-equal parts
## snow::clusterSplit(cl, seq)
length(cl)
clusterSplit(cl, 1:8)

## problems: not equal performance of workers (different machines)
## or different computing time (size) of problems
## dcpar::clusterSplitSB
clusterSplitSB(cl, 1:8, size=1)
clusterSplitSB(cl, 1:8, size=1:8)

## how many workers are needed?
## dcpar::clusterSize
clusterSize(1:5)
n <- 3
opar <- par(mfrow=c(2,2))
plotClusterSize(n,1:5,"none")
plotClusterSize(n,1:5,"load")
plotClusterSize(n,1:5,"size")
plotClusterSize(n,1:5,"both")
par(opar)

## snow::clusterApply(cl, seq, fun, ...)
clusterApply(cl, 1:2, sum, 3)

## snow::clusterApplyLB(cl, seq, fun, ...)
clusterApply(cl, 1:3, sum, 3)

## snow::clusterExport(cl, list)
dat <- matrix(1:10, 5, 2)
dat
clusterExport(cl, "dat")
clusterApply(cl, 1:2, function(i) dat[,i]+1)

## parLapply(cl, X, fun, ...)
dat <- list(1, 2, 3, 4)
parLapply(cl, dat, function(z) z^2)
## it is equivalent of
SEQ <- clusterSplit(cl, dat)
SEQ
res <- clusterApply(cl, SEQ, lapply, function(z) z^2)
docall(c, res)
## by using clusterSplitSB
SEQ <- clusterSplitSB(cl, dat, 1:4)
SEQ
res <- clusterApply(cl, SEQ, lapply, function(z) z^2)
docall(c, res)
## result is not really what we wanted
## so size balancing version of parLapply is implemented
parLapplySB(cl, dat, function(z) z^2, size=1:4)

stopCluster(cl)

## - no balancing: clusterApply, parLapply
## - load balancing: clusterApplyLB
## - size balancing: parLapplySB

## 2. parallel simulations/bootstrap

set.seed(1234)
n <- 20
x <- runif(n, -1, 1)
X <- model.matrix(~x)
beta <- c(2, -1)
mu <- X %*% beta
Y <- rpois(n, exp(mu))
m <- glm(Y ~ x, family=poisson)

## parametric bootstrap simulations
dat <- lapply(1:100, function(i) rpois(n, drop(exp(X %*% coef(m)))))
fun <- function(z) coef(glm(z ~ x, family=poisson))
coefs <- lapply(dat, fun)
coef(m)
summary(sapply(coefs, function(z) z[1]))
summary(sapply(coefs, function(z) z[2]))

## let us do this in parallel
cl <- makeCluster(2, type = "SOCK")
clusterEvalQ(cl, library(stats))
clusterExport(cl, c("m","n","x"))
## no balancing
coefs1 <- parLapply(cl, dat, fun)
coef(m)
summary(sapply(coefs1, function(z) z[1]))
summary(sapply(coefs1, function(z) z[2]))
## load balancing
coefs2 <- clusterApplyLB(cl, dat, fun)
coef(m)
summary(sapply(coefs2, function(z) z[1]))
summary(sapply(coefs2, function(z) z[2]))

## problem: when using snow functions from within a function,
## newly created objects are not in the global env
## solution: a wrapper for these common tasks:
coefs3 <- snowWrapper(cl, dat, fun, dat, name = "dat", lib = "stats")
coef(m)
summary(sapply(coefs3, function(z) z[1]))
summary(sapply(coefs3, function(z) z[2]))
## what is more: size & balancing, seed arguments are available
fun <- function(i) cldata$a * i - cldata$b
cldata <- list(a=10, b=5)
lapply(1:5, fun)
snowWrapper(cl, 1:5, fun, cldata)
snowWrapper(cl, 1:5, fun, cldata, balancing="load")
snowWrapper(cl, 1:5, fun, cldata, balancing="size", size=1:5)

stopCluster(cl)

## 3. parallel chains

model <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- inprod(X[i,], beta[1,])
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
}
data <- list(Y=Y, X=X, n=n, np=ncol(X))
params <- "beta"
m <- jags.fit(data, params, model)

n.chains <- 3

initsval <- suppressWarnings(jags.fit(data, params, model, n.adapt=1, n.iter=1, updated.model=FALSE))
inits <- lapply(lapply(initsval, as.list), function(z) {
    names(z) <- varnames(initsval)
    z})
rng <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")
seed <- 99*1:n.chains
for (i in 1:n.chains) {
    inits[[i]][[".RNG.name"]] <- rng[i]
    inits[[i]][[".RNG.seed"]] <- seed[i]
}

cldata <- list(data=data, params=params, model=model, inits=inits)

jagsparallel <- function(i, ...)   {
    jags.fit(data=cldata$data, params=cldata$params, model=cldata$model,
    inits=cldata$inits[[i]], n.chains=1, updated.model=FALSE, ...)
}

summary(jagsparallel(1))

cl <- makeCluster(3, type = "SOCK")

mcmc <- snowWrapper(cl, 1:n.chains, jagsparallel, cldata, lib="dcpar", 
    balancing="none", size=1, seed=100*1:length(cl))
res <- as.mcmc.list(lapply(mcmc, as.mcmc))
res[1:2,]
## chains are not identical
summary(res)
gelman.diag(res)

mm <- jags.parfit(cl, data, params, model)
mm[1:2,]
summary(mm)
gelman.diag(mm)

stopCluster(cl)

## a clean parallel session

library(dcpar)
set.seed(1234)
n <- 20
x <- runif(n, -1, 1)
X <- model.matrix(~x)
beta <- c(2, -1)
mu <- X %*% beta
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
pm[1:2,]
summary(pm)
gelman.diag(pm)
stopCluster(cl)

## 4. iterative parallel data cloning

dat <- list(Y=Y, X=X, n=n, np=ncol(X))
## no. of clones
k <- 1:3
## clone the data, fit the model, calculate statistics, and return the k.max model
dcfun <- function(i, ...) {
    dcdat <- dclone(dat, k[i], multiply="n", unchanged="np")
    m <- jags.fit(dcdat, "beta", glm.model, ...)
    if (i==length(k))
        return(m) else return(list(dct=dctable(m), dcd=dcdiag(m)))
}
dcm <- lapply(1:length(k), dcfun)
dcm[1:2]
summary(dcm[[3]])
str(dcm)

## optimize the number of clusters
clusterSize(1:3)
plotClusterSize(2, 1:3, "size")
cl <- makeCluster(2, type = "SOCK")

clusterEvalQ(cl, library(dcpar))
clusterExport(cl, c("dat", "glm.model", "dcfun", "k"))

pdcm <- parLapplySB(cl, 1:length(k), k, dcfun)
pdcm[1:2]
summary(pdcm[[3]])
str(pdcm)

## this is implemented in dc.parfit
dcmod <- dc.fit(dat, "beta", glm.model, n.clones=k, multiply="n", unchanged="np")
pdcmod <- dc.parfit(cl, dat, "beta", glm.model, n.clones=k, multiply="n", unchanged="np")

stopCluster(cl)

## The End



## dclone documentation checks for dontrun pieces
library(dclone)
## ?bugs.fit #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
inits <- function(){
    list(theta=rnorm(nrow(schools), 0, 100), mu.theta=rnorm(1, 0, 100),
         sigma.theta=runif(1, 0, 100))
}
param <- c("mu.theta", "sigma.theta")
sim <- bugs.fit(dat, param, bugs.model, inits)
dat2 <- dclone(dat, 2, multiply="J")
sim2 <- bugs.fit(dat2, param, bugs.model)
## fitting the model with OpenBUGS
sim3 <- bugs.fit(dat2, param, bugs.model, program="openbugs", n.thin=1)
## fitting the model with JAGS
sim4 <- jags.fit(dat2, param, bugs.model)

## ?bugs.parfit #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
cl <- makeSOCKcluster(3)
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

## ?clusterSplitSB #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cl <- makeSOCKcluster(2)
## equal sizes, same as clusterSplit(cl, 1:5)
clusterSplitSB(cl, 1:5)
## different sizes
clusterSplitSB(cl, 1:5, 5:1)
x <- list(1, 2, 3, 4)
parLapplySB(cl, x, function(z) z^2, size=1:4)
stopCluster(cl)

## ?coef.mcmc.list #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
jfun <- function() {
    for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], tau)
        mu[i] <- alpha + beta * (x[i] - x.bar)
    }
    x.bar <- mean(x)
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
linpred <- model.matrix(~x) %*% c(alpha, beta)
Y <- rnorm(N, mean = linpred, sd = sigma)
## data for the model
dcdata <- dclone(list(N = N, Y = Y, x = x), 5, multiply = "N")
## data cloning
dcmod <- jags.fit(dcdata, c("alpha", "beta", "sigma"), jfun, n.chains = 3)
summary(dcmod)
coef(dcmod)
dcsd(dcmod)
confint(dcmod)
vcov(dcmod)
vcov(dcmod, invfisher = FALSE)
quantile(dcmod)

## ?dc.fit #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(1234)
n <- 20
beta <- c(2, -1)
sigma <- 0.1
alpha <- rnorm(n, 0, sigma)
x <- runif(n)
X <- model.matrix(~x)
linpred <- X %*% beta + alpha
Y <- rpois(n, exp(linpred))
## JAGS model as a function
jfun1 <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
    sigma ~ dlnorm(0, 0.001)
}
## data
jdata <- list(n = n, Y = Y, X = X, np = NCOL(X))
## number of clones to be used, etc.
## iteartive fitting
jmod <- dc.fit(jdata, c("beta", "sigma"), jfun1, 
    n.clones = 1:5, multiply = "n", unchanged = "np")
## summary with DC SE and R hat
summary(jmod)
dct <- dctable(jmod)
plot(dct)
## How to use estimates to make priors more informative?
glmm.model.up <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:p) {
        beta[1,j] ~ dnorm(priors[j,1], priors[j,2])
    }
    sigma ~ dgamma(priors[(p+1),2], priors[(p+1),1])
}
## function for updating, x is an MCMC object
upfun <- function(x) {
    if (missing(x)) {
        p <- ncol(X)
        return(cbind(c(rep(0, p), 0.001), rep(0.001, p+1)))
    } else {
        par <- coef(x)
        return(cbind(par, rep(0.01, length(par))))
    }
}
updat <- list(n = n, Y = Y, X = X, p = ncol(X), priors = upfun())
dcmod <- dc.fit(updat, c("beta", "sigma"), glmm.model.up,
    n.clones = 1:5, multiply = "n", unchanged = "p",
    update = "priors", updatefun = upfun)
summary(dcmod)
## time series example
## data and model taken from Ponciano et al. 2009
## Ecology 90, 356-362.
paurelia <- c(17,29,39,63,185,258,267,392,510,570,650,560,575,650,550,480,520,500)
dat <- list(ncl=1, n=length(paurelia), Y=dcdim(data.matrix(paurelia)))
beverton.holt <- function() {
    for (k in 1:ncl) {
        for(i in 2:(n+1)){
            ## observations
            Y[(i-1), k] ~ dpois(exp(X[i, k]))
            ## state
            X[i, k] ~ dnorm(mu[i, k], 1 / sigma^2)
            mu[i, k] <- X[(i-1), k] + log(lambda) - log(1 + beta * exp(X[(i-1), k]))
        }
        ## state at t0
        X[1, k] ~ dnorm(mu0, 1 / sigma^2)
    }
    # Priors on model parameters
    beta ~ dlnorm(-1, 1)
    sigma ~ dlnorm(0, 1)
    tmp ~ dlnorm(0, 1)
    lambda <- tmp + 1
    mu0 <- log(2)  + log(lambda) - log(1 + beta * 2)
}
mod <- dc.fit(dat, c("lambda","beta","sigma"), beverton.holt,
    n.clones=c(2, 5, 10), multiply="ncl", unchanged="n")
## compare with results from the paper:
##   beta   = 0.00235
##   lambda = 2.274
##   sigma  = 0.1274
summary(mod)

## Using WinBUGS/OpenBUGS
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
inits <- function(){
    list(theta=rnorm(nrow(schools), 0, 100), mu.theta=rnorm(1, 0, 100),
         sigma.theta=runif(1, 0, 100))
}
param <- c("mu.theta", "sigma.theta")
sim2 <- dc.fit(dat, param, bugs.model, n.clones=1:2, 
    flavour="bugs", program="WinBUGS", multiply="J")
sim3 <- dc.fit(dat, param, bugs.model, n.clones=1:2, 
    flavour="bugs", program="OpenBUGS", multiply="J")

## ?dc.parfit #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
k <- 1:3
cl <- makeCluster(2, type = "SOCK")
dcm <- dc.fit(dat, "beta", glm.model, n.clones=k, multiply="n", unchanged="np")
pdcm <- dc.parfit(cl, dat, "beta", glm.model, n.clones=k, multiply="n", unchanged="np")
summary(dcm)
summary(pdcm)
stopCluster(cl)

## Using WinBUGS/OpenBUGS
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
inits <- function(){
    list(theta=rnorm(nrow(schools), 0, 100), mu.theta=rnorm(1, 0, 100),
         sigma.theta=runif(1, 0, 100))
}
param <- c("mu.theta", "sigma.theta")
cl <- makeCluster(2, type = "SOCK")
sim2 <- dc.parfit(cl, dat, param, bugs.model, n.clones=1:2, 
    flavour="bugs", program="WinBUGS", multiply="J")
sim3 <- dc.parfit(cl, dat, param, bugs.model, n.clones=1:2, 
    flavour="bugs", program="OpenBUGS", multiply="J")
stopCluster(cl)

## ?dctable #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(1234)
n <- 20
beta <- c(2, -1)
sigma <- 0.1
alpha <- rnorm(n, 0, sigma)
x <- runif(n)
X <- model.matrix(~x)
linpred <- X %*% beta + alpha
Y <- rpois(n, exp(linpred))
## JAGS model as a function
jfun1 <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
    sigma ~ dlnorm(0, 0.001)
}
## data
jdata <- list(n = n, Y = Y, X = X, np = NCOL(X))
## number of clones to be used, etc.
## iteartive fitting
jmod <- dc.fit(jdata, c("beta", "sigma"), jfun1, 
    n.clones = 1:5, multiply = "n", unchanged = "np")
## summary with DC SE and R hat
summary(jmod)
dct <- dctable(jmod)
plot(dct)
## How to use estimates to make priors more informative?
glmm.model.up <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:p) {
        beta[1,j] ~ dnorm(priors[j,1], priors[j,2])
    }
    sigma ~ dgamma(priors[(p+1),2], priors[(p+1),1])
}
## function for updating, x is an MCMC object
upfun <- function(x) {
    if (missing(x)) {
        p <- ncol(X)
        return(cbind(c(rep(0, p), 0.001), rep(0.001, p+1)))
    } else {
        par <- coef(x)
        return(cbind(par, rep(0.01, length(par))))
    }
}
updat <- list(n = n, Y = Y, X = X, p = ncol(X), priors = upfun())
dcmod <- dc.fit(updat, c("beta", "sigma"), glmm.model.up,
    n.clones = 1:5, multiply = "n", unchanged = "p",
    update = "priors", updatefun = upfun)
summary(dcmod)
dct <- dctable(dcmod)
plot(dct)
plot(dct, type = "var")

## ?jags.fit #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
linpred <- model.matrix(~x) %*% c(alpha, beta)
Y <- rnorm(N, mean = linpred, sd = sigma)
## list of data for the model
jdata <- list(N = N, Y = Y, x = x)
## what to monitor
jpara <- c("alpha", "beta", "sigma")
## fit the model with JAGS
regmod <- jags.fit(jdata, jpara, jfun, n.chains = 3)
## model summary
summary(regmod)
## data cloning
dcdata <- dclone(jdata, 5, multiply = "N")
dcmod <- jags.fit(dcdata, jpara, jfun, n.chains = 3)
summary(dcmod)

## ?jags.parfit #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
pm4 <- jags.parfit(cl, dat, "beta", glm.model, inits)
stopCluster(cl)

## ?snowWrapper #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cl <- makeSOCKcluster(2)
## wrapper
fun <- function(i) cldata$a * i - cldata$b
cldata <- list(a=10, b=5)
snowWrapper(cl, 1:5, fun, cldata)
stopCluster(cl)

## ?update.mcmc.list #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
linpred <- model.matrix(~x) %*% c(alpha, beta)
Y <- rnorm(N, mean = linpred, sd = sigma)
## list of data for the model
jdata <- list(N = N, Y = Y, x = x)
## what to monitor
jpara <- c("alpha", "beta", "sigma")
## fit the model with JAGS
regmod <- jags.fit(jdata, jpara, jfun, n.chains = 3)
## get the updated model
upmod <- updated.model(regmod)
upmod
## automatic updating
## using R_hat < 1.1 as criteria
critfun <- function(x)
    all(gelman.diag(x)$psrf[,1] < 1.1)
mod <- update(regmod, critfun, 5)
## update just once
mod2 <- update(regmod)
summary(mod)

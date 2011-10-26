library(dcmle)

## smallest example ever

Y <- 4
summary(gm <- glm(Y ~ 1, family=poisson))
exp(coef(gm))
fun1 <- function(y) sum(dpois(Y, exp(y), log=TRUE))
parm <- seq(0, 2, len=101)
llik <- sapply(parm, fun1)
par(mfrow=c(2,1), las=1)
plot(parm, llik, type="l", col=4, ylab="log Likelihood", xlab="Estimate")
abline(v=coef(glm(Y ~ 1, family=poisson)), col=2)
plot(parm, exp(llik), type="l", col=4, ylab="Likelihood", xlab="Estimate")
abline(v=coef(glm(Y ~ 1, family=poisson)), col=2)

## ordinary Bayesian JAGS session

glm.pois <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- inprod(X[i,], beta)
    }
    for (j in 1:np) {
        beta[j] ~ dnorm(0, 0.001)
    }
}
X <- matrix(1, 1, 1)
d <- list(Y=Y, n=length(Y), X=X, np=ncol(X))
fn <- write.jags.model(glm.pois)
(jm <- jags.model(fn, d, n.chains=3))
update(jm, n.iter=1000)
m <- coda.samples(jm, "beta", n.iter=1000, thin=1)
clean.jags.model(fn)
summary(m)
plot(m)

## jags.fit: a wrapper
m <- jags.fit(d, "beta", glm.pois, n.iter=1000)
summary(m)
plot(m)
updated.model(m)
coef(m)

## effect of prior specifications

glm.pois2 <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- inprod(X[i,], beta)
    }
    for (j in 1:np) {
        beta[j] ~ dnorm(mu, prec)
    }
}
vals <- expand.grid(mu=c(-5, 0, 5), prec=c(0.001, 1))
d2 <- apply(vals, 1, function(z) 
    list(n = length(Y), Y = Y, X = X, np = ncol(X), mu=z[1], prec=z[2]))
m <- lapply(d2, function(z) 
    jags.fit(z, "beta", glm.pois2, n.iter = 1000))
res <- data.frame(vals, 
    est=sapply(m, coef), 
    std.err=sapply(m, dcsd))
res$bias <- res$est - coef(gm)
res

## data cloning

k <- 20
dk <- list(Y=rep(Y, k), n=length(Y)*k, X=matrix(X,nrow(X)*k,ncol(X)), np=ncol(X))
fn <- write.jags.model(glm.pois)
(jm <- jags.model(fn, dk, n.chains=3))
update(jm, n.iter=1000)
m <- coda.samples(jm, "beta", n.iter=1000, thin=1)
clean.jags.model(fn)
summary(m)
plot(m)

## dclone

dk <- dclone(d, n.clones=k, multiply="n", unchanged="np")
nclones(dk)
fn <- write.jags.model(glm.pois)
(jm <- jagsModel(fn, dk, n.chains=3))
nclones(jm)
update(jm, n.iter=1000)
m <- codaSamples(jm, "beta", n.iter=1000, thin=1)
clean.jags.model(fn)
summary(m)
nclones(m)

## via jags.fit

m <- jags.fit(dk, "beta", glm.pois, n.iter=1000)
summary(m)
nclones(m)

## DC and priors

m2 <- lapply(d2, function(z) 
    jags.fit(dclone(z, 100, multiply="n", 
    unchanged=c("np","mu","prec")), "beta", 
    glm.pois2, n.iter = 1000))
res2 <- data.frame(vals, 
    est=sapply(m2, coef), 
    std.err=sapply(m2, dcsd))
res2$bias <- res2$est - coef(gm)
res2

plot(res$bias, type="b", col=2, lwd=2)
abline(0, 0, lty=2)
points(res2$bias, type="b", col=4, lwd=2)

## diagnostics

res3 <- dc.fit(d2[[4]], "beta", glm.pois2, n.iter = 1000,
    n.clones=c(1, 5, 10, 25, 50, 100), 
    multiply="n", unchanged=c("np","mu","prec"))
coef(summary(gm))
c(est=coef(res3), std.err=dcsd(res3))
dcdiag(res3)
plot(dcdiag(res3))
plot(dctable(res3))


## more data is always better

n <- 20
set.seed(1234)
Y <- rpois(n, 4)
X <- matrix(1, n, 1)
vals <- expand.grid(mu=c(-5, 0, 5), prec=c(0.001, 1))
d <- apply(vals, 1, function(z) list(n = length(Y), Y = Y, X = X, np = ncol(X), mu=z[1], prec=z[2]))
m <- lapply(d, function(z) jags.fit(z, "beta", glm.pois2, n.iter = 1000))
res4 <- data.frame(vals, est=sapply(m, coef), std.err=sapply(m, dcsd))
res4$bias <- res4$est - coef(glm(Y ~ 1, family=poisson))
res4

plot(res$bias, type="b", col=2, lwd=2)
abline(0, 0, lty=2)
points(res2$bias, type="b", col=4, lwd=2)
points(res4$bias, type="b", col=3, lwd=2)

res5 <- dc.fit(d[[4]], "beta", glm.pois2, n.iter = 1000,
    n.clones=c(1, 5, 10, 25, 50, 100), 
    multiply="n", unchanged=c("np","mu","prec"))

coef(summary(glm(Y ~ 1, family=poisson)))
c(est=coef(res5), std.err=dcsd(res5))
dcdiag(res5)
plot(dcdiag(res5))
plot(dctable(res5))

## we happen to have a covariate

x <- rnorm(n)
X <- model.matrix(~x)
Y <- rpois(n, exp(X %*% c(1, -0.5)))
gm <- glm(Y ~ x, family=poisson)
vals <- expand.grid(b0=seq(0,2,len=101), b1=seq(-1,1,len=101))
fun2 <- function(z) sum(dpois(Y, exp(X %*% z), log=TRUE))
tp <- apply(vals, 1, fun2)
L <- matrix(exp(tp), 101, 101)
image(seq(0,2,len=101), seq(-1,1,len=101), max(L)-L,
    xlab=expression(beta[0]), ylab=expression(beta[1]))
abline(v=coef(glm(Y ~ x, family=poisson))[1], 
    h=coef(glm(Y ~ x, family=poisson))[2], col=4)

dat <- list(n = length(Y), 
    Y = Y, 
    X = X, 
    np = ncol(X), 
    mu=0, prec=0.001)
res6 <- dc.fit(dat, "beta", glm.pois2, n.iter = 1000,
    n.clones=c(1, 5, 10, 25, 50, 100), 
    multiply="n", unchanged=c("np","mu","prec"))

coef(summary(gm))
cbind(est=coef(res6), std.err=dcsd(res6))
dcdiag(res6)
plot(dcdiag(res6))
plot(dctable(res6))

## could we get a fancy summary, please?

dcf <- makeDcFit(data=dat,
    model=glm.pois2,
    params="beta",
    multiply="n", 
    unchanged=c("np","mu","prec"))
res7 <- dcmle(dcf, n.clones=c(1, 5, 10, 25, 50, 100),
    n.iter=1000)

summary(gm)
res7
vcov(gm)
vcov(res7)
confint(gm)
confint(res7)

## custommodel
print(custommodel(glm.pois2), deparse=TRUE)
dcf@model <- structure(
c(" model { ",
"     for (i in 1:n) { ",
"         Y[i] ~ dpois(lambda[i]) ",
"         log(lambda[i]) <- inprod(X[i,], beta) ",
"     } ",
"     for (j in 1:np) { ",
"         beta[j] ~ dnorm(mu, prec) ",
"     } ",
" } "),
class = "custommodel")


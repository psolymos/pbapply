###################################################
### chunk number 1: 
###################################################
library(dcpar)
options(prompt = "R> ", continue = "+   ", useFancyQuotes = FALSE, width = 76)
setwd("c:/svn/abmiserver/dcpar-paper")
load("parallel.results2.Rdata")


###################################################
### chunk number 2: 
###################################################
n <- 200
beta <- c(1, -1)
sigma <- 0.25
set.seed(1234)
x <- runif(n)
X <- model.matrix(~x)
mu <- rnorm(n, mean=drop(X %*% beta), sd=sigma)
Y <- rpois(n, exp(mu))


###################################################
### chunk number 3: 
###################################################
dat <- list(Y = Y, X = X, n = n, np = NCOL(X))


###################################################
### chunk number 4: 
###################################################
glmm.model <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(exp(mu[i]))
        mu[i] ~ dnorm(inprod(X[i,], beta), 1/exp(log.sigma)^2)
    }
    for (j in 1:np) {
        beta[j] ~ dnorm(0, 0.001)
    }
    log.sigma ~ dnorm(0, 0.001)
}


###################################################
### chunk number 5: 
###################################################
n.adapt <- 2000
n.update <- 8000
n.iter <- 2000


###################################################
### chunk number 6: 
###################################################
k <- c(1, 2, 5, 10, 20)


###################################################
### chunk number 7: 
###################################################
timerfitfun <- function(parallel = FALSE, ...) {
    t0 <- proc.time()
    mod <- if (parallel)
        jags.parfit(...) else jags.fit(...)
    attr(mod, "timer") <- (proc.time() - t0)[3] / 60
    mod
}


###################################################
### chunk number 8:  eval=FALSE
###################################################
## res1 <- lapply(k, function(z)
##     timerfitfun(parallel = FALSE,
##         data = dclone(dat, z, multiply = "n", unchanged = "np"),
##         params = c("beta", "log.sigma"),
##         model = glmm.model,
##         n.adapt = n.adapt, n.update = n.update, n.iter = n.iter))


###################################################
### chunk number 9:  eval=FALSE
###################################################
## cl <- makeCluster(3, type = "SOCK")
## res2 <- lapply(k, function(z)
##     timerfitfun(parallel = TRUE,
##         cl = cl,
##         data = dclone(dat, z, multiply = "n", unchanged = "np"),
##         params = c("beta", "log.sigma"), 
##         model = glmm.model,
##         n.adapt = n.adapt, n.update = n.update, n.iter = n.iter))
## stopCluster(cl)


###################################################
### chunk number 10: 
###################################################
pt1 <- sapply(res1, function(z) attr(z, "timer"))
pt2 <- sapply(res2, function(z) attr(z, "timer"))


###################################################
### chunk number 11: 
###################################################
tab1 <- data.frame(n.clones=k,
    time=pt1, 
    rel.change = pt1 / pt1[1],
    time.par=pt2, 
    rel.change.par = pt2 / pt2[1])
round(tab1, 2)


###################################################
### chunk number 12: 
###################################################
clusterSize(pt1)


###################################################
### chunk number 13: balancing
###################################################
opar <- par(mfrow=c(1,3), cex.lab=1.5, cex.main=1.5, cex.sub=1.5)
col <- heat.colors(length(k))
plotClusterSize(2, pt1, "none", col = col, xlim = c(0, 20))
plotClusterSize(2, pt1, "load", col = col[c(1, 3, 5, 2, 4)], xlim = c(0, 20))
plotClusterSize(2, pt1, "size", col = col[c(5, 4, 3, 2, 1)], xlim = c(0, 20))
par(opar)


###################################################
### chunk number 14:  eval=FALSE
###################################################
## t0 <- proc.time()
## res3 <- dc.fit(dat, c("beta", "log.sigma"), glmm.model,
##     n.clones = k, multiply = "n", unchanged = "np",
##     n.update = n.update, n.iter = n.iter)
## attr(res3, "timer") <- (proc.time() - t0)[3] / 60


###################################################
### chunk number 15:  eval=FALSE
###################################################
## cl <- makeCluster(2, type = "SOCK")
## t0 <- proc.time()
## res4 <- dc.parfit(cl, dat, c("beta", "log.sigma"), glmm.model,
##     n.clones = k, multiply = "n", unchanged = "np",
##     n.update = n.update, n.iter = n.iter)
## attr(res4, "timer") <- (proc.time() - t0)[3] / 60
## stopCluster(cl)


###################################################
### chunk number 16: estimates
###################################################
dct1 <- dctable(res1[[1]], res1[[2]], res1[[3]], res1[[4]], res1[[5]])
dct2 <- dctable(res2[[1]], res2[[2]], res2[[3]], res2[[4]], res2[[5]])
dct3 <- dctable(res3)
dct4 <- dctable(res4)
opar <- par(mfrow=c(1,3))
for (i in 1:3) {
    main <- list(expression(beta[0]), expression(beta[1]), expression(log(sigma)))
    truth <- c(beta, log(sigma))
    MEAN <- cbind(dct1[[i]]$mean, dct2[[i]]$mean, dct3[[i]]$mean, dct4[[i]]$mean)
    SD <- cbind(dct1[[i]]$sd, dct2[[i]]$sd, dct3[[i]]$sd, dct4[[i]]$sd)
    xval <- cbind(1:5-0.24, 1:5-0.08, 1:5+.08, 1:5+0.24)
    plot(xval, MEAN, ylim=range(MEAN+SD, MEAN-SD), xlim=c(0.5, 5.5), type="n",
        xlab = "Number of clones", ylab="Estimates",
        main = main[[i]], axes = FALSE)
    abline(h=truth[i], lty=2)
    errlines(array(xval), cbind(array(MEAN - SD), array(MEAN + SD)), col=rep(1:4, each=5))
    points(xval, MEAN, col=rep(1:4, each=5), pch=19)
    axis(1, 1:5, k)
    axis(2)
    box()
}
par(opar)



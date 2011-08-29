###################################################
### chunk number 1: settings
###################################################
#line 59 "dcpar3.Rnw"
library(dclone)
options(prompt = "R> ", continue = "+   ", useFancyQuotes = FALSE, width = 76)
setwd("c:/Dropbox/pkg/dcpar-paper/resubm")#;Sweave("dcpar3.Rnw")
load("dcpar3.Rdata")


###################################################
### chunk number 2: dataset
###################################################
#line 111 "dcpar3.Rnw"
source("http://dcr.r-forge.r-project.org/examples/seeds.R")
str(dat <- seeds$data)


###################################################
### chunk number 3: bugsmodel
###################################################
#line 126 "dcpar3.Rnw"
(model <- seeds$model)


###################################################
### chunk number 4: inits
###################################################
#line 130 "dcpar3.Rnw"
str(inits <- seeds$inits)


###################################################
### chunk number 5: mcmcset
###################################################
#line 139 "dcpar3.Rnw"
n.adapt <- 1000
n.update <- 1000
n.iter <- 3000
thin <- 10
n.chains <- 3
params <- c("alpha0", "alpha1", "alpha2", "alpha12", "sigma")


###################################################
### chunk number 6: mod1 eval=FALSE
###################################################
## #line 148 "dcpar3.Rnw"
## m <- jags.fit(data = dat, params = params, model = model, 
##     inits = inits, n.adapt = n.adapt, n.update = n.update, 
##     n.iter = n.iter, thin = thin, n.chains = n.chains)


###################################################
### chunk number 7: cl
###################################################
#line 194 "dcpar3.Rnw"
library(snow)
cl <- makeCluster(3, type = "SOCK")


###################################################
### chunk number 8: inits2 eval=FALSE
###################################################
## #line 203 "dcpar3.Rnw"
## inits2 <- jags.fit(dat, params, model, inits, n.chains,
##     n.adapt = 0, n.update = 0, n.iter = 0)$state(internal = TRUE)


###################################################
### chunk number 9: writemod
###################################################
#line 214 "dcpar3.Rnw"
clusterEvalQ(cl, library(dclone))
clusterEvalQ(cl, setwd(getwd()))
filename <- write.jags.model(model)


###################################################
### chunk number 10: clexport
###################################################
#line 229 "dcpar3.Rnw"
cldata <- list(data=dat, params=params, model=filename, inits=inits2)
clusterExport(cl, "cldata")


###################################################
### chunk number 11: jagsparallel
###################################################
#line 234 "dcpar3.Rnw"
jagsparallel <- function(i, ...)   {
    jags.fit(data = cldata$data, params = cldata$params, model = cldata$model, 
        inits = cldata$inits[[i]], n.chains = 1, updated.model = FALSE, ...)
}


###################################################
### chunk number 12: parfit eval=FALSE
###################################################
## #line 242 "dcpar3.Rnw"
## res <- parLapply(cl, 1:n.chains, jagsparallel, 
##     n.adapt = n.adapt, n.update = n.update, n.iter = n.iter, thin = thin)
## res <- as.mcmc.list(lapply(res, as.mcmc))


###################################################
### chunk number 13: cleanup
###################################################
#line 247 "dcpar3.Rnw"
clean.jags.model(filename)


###################################################
### chunk number 14: jagsparfit eval=FALSE
###################################################
## #line 253 "dcpar3.Rnw"
## m2 <- jags.parfit(cl, data = dat, params = params, model = model, 
##     inits = inits, n.adapt = n.adapt, n.update = n.update, 
##     n.iter = n.iter, thin = thin, n.chains = n.chains)
## stopCluster(cl)


###################################################
### chunk number 15: options
###################################################
#line 276 "dcpar3.Rnw"
dcoptions(RNG = "RNGstream")
dcoptions()$RNG


###################################################
### chunk number 16: timerfun
###################################################
#line 340 "dcpar3.Rnw"
timerfitfun <- function(parallel = FALSE, ...) {
    t0 <- proc.time()
    mod <- if (parallel)
        jags.parfit(...) else jags.fit(...)
    attr(mod, "timer") <- (proc.time() - t0)[3] / 60
    mod
}


###################################################
### chunk number 17: nclones
###################################################
#line 350 "dcpar3.Rnw"
n.clones <- c(1, 2, 5, 10, 25)


###################################################
### chunk number 18: res1 eval=FALSE
###################################################
## #line 356 "dcpar3.Rnw"
## res1 <- lapply(n.clones, function(z)
##     timerfitfun(parallel = FALSE,
##         data = dclone(dat, n.clones= z, multiply = "N"),
##         params = params,
##         model = model,
##         inits = inits,
##         n.adapt = n.adapt, 
##         n.update = n.update, 
##         n.iter = n.iter,
##         thin = thin,
##         n.chains = n.chains))


###################################################
### chunk number 19: res2 eval=FALSE
###################################################
## #line 372 "dcpar3.Rnw"
## cl <- makeCluster(3, type = "SOCK")
## res2 <- lapply(n.clones, function(z)
##     timerfitfun(parallel = TRUE,
##         cl = cl,
##         data = dclone(dat, n.clones = z, multiply = "N"),
##         params = params,
##         model = model,
##         inits = inits,
##         n.adapt = n.adapt, 
##         n.update = n.update, 
##         n.iter = n.iter,
##         thin = thin,
##         n.chains = n.chains))
## stopCluster(cl)


###################################################
### chunk number 20: pt12
###################################################
#line 392 "dcpar3.Rnw"
pt1 <- sapply(res1, function(z) attr(z, "timer"))
pt2 <- sapply(res2, function(z) attr(z, "timer"))


###################################################
### chunk number 21: tab1
###################################################
#line 400 "dcpar3.Rnw"
tab1 <- data.frame(n.clones=n.clones,
    time=pt1, 
    rel.change = pt1 / pt1[1],
    time.par=pt2, 
    rel.change.par = pt2 / pt2[1],
    speedup = pt2 / pt1)
round(tab1, 2)


###################################################
### chunk number 22: split
###################################################
#line 431 "dcpar3.Rnw"
clusterSplit(1:2, n.clones)


###################################################
### chunk number 23: splitSB
###################################################
#line 447 "dcpar3.Rnw"
clusterSplitSB(1:2, n.clones, size = n.clones)


###################################################
### chunk number 24: clsize
###################################################
#line 470 "dcpar3.Rnw"
clusterSize(n.clones)


###################################################
### chunk number 25: balancing
###################################################
#line 483 "dcpar3.Rnw"
opar <- par(mfrow=c(1,3), cex.lab=1.5, cex.main=1.5, cex.sub=1.5)
col <- heat.colors(length(n.clones))
xlim <- c(0, max(clusterSize(n.clones)[2,-1]))
plotClusterSize(2, n.clones, "none", col = col, xlim = xlim)
plotClusterSize(2, n.clones, "load", col = col, xlim = xlim)
plotClusterSize(2, n.clones, "size", col = col, xlim = xlim)
par(opar)


###################################################
### chunk number 26: res3 eval=FALSE
###################################################
## #line 510 "dcpar3.Rnw"
## t0 <- proc.time()
## res3 <- dc.fit(dat, params, model, inits,
##     n.clones = n.clones, multiply = "N",
##     n.adapt = n.adapt, n.update = n.update, 
##     n.iter = n.iter, thin = thin, n.chains = n.chains)
## attr(res3, "timer") <- (proc.time() - t0)[3] / 60


###################################################
### chunk number 27: res4 eval=FALSE
###################################################
## #line 525 "dcpar3.Rnw"
## cl <- makeCluster(2, type = "SOCK")
## t0 <- proc.time()
## res4 <- dc.parfit(cl, dat, params, model, inits,
##     n.clones = n.clones, multiply = "N",
##     n.adapt = n.adapt, n.update = n.update, 
##     n.iter = n.iter, thin = thin, n.chains = n.chains)
## attr(res4, "timer") <- (proc.time() - t0)[3] / 60
## stopCluster(cl)



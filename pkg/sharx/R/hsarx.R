#http://cran.r-project.org/web/packages/Formula/vignettes/Formula.pdf
hsarx <- 
function(formula, data, n.clones, cl=NULL, subset, na.action, stage=0, ...)
{
    if (missing(n.clones))
        stop("'n.clones' argument missing")
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
    mf <- mf[c(1, m)]
    f <- Formula(formula)
    st <- length(f)
    if (st[1] != 1)
        stop("multiple responses in LHS are not allowed")
    mf[[1]] <- as.name("model.frame")
    mf$formula <- f
    mf <- eval(mf, parent.frame())
    Y <- model.response(mf)
    X <- model.matrix(f, data = mf, rhs = 1)
    if (st[2] > 1) {
        Z <- model.matrix(f, data = mf, rhs = 2)
        if (length(formula(f, lhs=FALSE, rhs=3)[[2]]) > 1)
            stop("inappropriate grouping variable")
        G <- model.matrix(f, data = mf, rhs = 3)
        if (ncol(G) > 2) {
            G[rowSums(G[,-1]) != 0,1] <- 0
            G <- rowSums(col(G) * G)
        } else {
            G <- G[,2]
        }
        G <- as.integer(as.factor(G))
        if (length(unique(G)) == 1)
            stop("grouping variable must have at least 2 levels")
    } else {
        Z <- NULL
        G <- NULL
    }
    if (stage == 1)
        return(list(Y=Y, X=X, Z=Z, G=G, n.clones=n.clones, cl=cl))
    out <- hsarx.fit(Y, X, Z, G, n.clones, cl, stage, ...)
#    class(out) <- "hsar"
#    out$formula <- f
#    out$model <- mf
    out
}

hsarx.fit <- 
function(Y, X, Z, G, n.clones, cl=NULL, stage, ...)
{
#    list(Y=Y, X=X, Z=Z, G=G, n.clones=n.clones, cl=cl)
    m <- length(Y) # no. of islands
    if (is.null(Z)) {
        n <- 1
        G <- rep(1, n)
    } else {
        n <- length(unique(G)) # no. of studies
    }
    p <- ncol(X) # no. of focal parameters
    dy <- lapply(1:n, function(i) Y[G == unique(G)[i]])
    dx <- lapply(1:n, function(i) X[G == unique(G)[i],])
    lmmods <- lapply(1:n, function(i) lm(dy[[i]] ~ dx[[i]]-1))
    if (!is.null(Z)) {
        ## HSAR/HSARX estimation
        q <- ncol(Z) # no. of modifiers
        dz <- sapply(1:n, function(i) data.matrix(Z[G == unique(G)[i],])[1,])
        dz <- if (q > 1)
            t(dz) else data.matrix(dz)
        ## weighted averaging meta analysis for priors
        lmmods <- lapply(1:n, function(i) lm(dy[[i]] ~ dx[[i]]-1))
        cfs <- t(sapply(lmmods, coef))
        ses <- t(sapply(lmmods, function(z) coef(summary(z))[,2]))
        lsig <- sapply(lmmods, function(z) log(summary(z)$sigma))
        tau2 <- sapply(1:p, function(i) {
            vwts <- 1/ses[,i]^2
            fixedsumm <- sum(vwts * cfs[,i]) / sum(vwts)
            Q <- sum(((cfs[,i] - fixedsumm)^2) / ses[,i]^2)
            tau2 <- max(0, (Q - n - 1)/(sum(vwts) - sum(vwts^2)/sum(vwts)))
        })
        w <- sapply(1:p, function(i) 1/(tau2[i] + ses[,i]^2))
        wm <- lapply(1:p, function(i) lm(cfs[,i] ~ dz-1, weights=w[,i]))
        wmsig <- lm(lsig ~ dz-1)
        wm[[(p+1)]] <- wmsig
        ## create objects for priors
        pr.cfs <- t(sapply(wm, coef))
#        pr.ses <- t(sapply(wm, function(z) 1/(coef(summary(z))[,2]^2))) ## too strong priors
        pr.ses <- rep(0.1, prod(dim(pr.cfs)))
        dim(pr.ses) <- dim(pr.cfs)
        pr.tau <- rbind(c(log(sqrt(tau2)), 0), rep(0.1, p+1))
        ZG <- Z[sapply(1:n, function(i) min(which(G == unique(G)[i]))),]
        dat <- list(logY=dcdim(data.matrix(Y)), X=X, ZG=ZG, G=G,
            n=n, m=m, p=p, q=q, ncl=1, 
            pr.cfs=pr.cfs, pr.ses=pr.ses, pr.tau=pr.tau)
if (stage == 2)
    return(dat)
        dimnames(dat$logY) <- NULL
        ## DC comes here
        hsarx.lmm <- function() {
            for (cl in 1:ncl) { # clones
                for (j in 1:m) { # islands
                    ## focal model
                    logY[j,cl] ~ dnorm(mu[j,cl], 1/exp(log.sigma.i[G[j],cl])^2)
                    mu[j,cl] <- inprod(X[j,], beta.i[G[j],,cl])
                }
                for (i in 1:n) { # studies
                    for (k in 1:p) { # focal parameters
                        ## modifier models for each focal parameter k
                        beta.i[i,k,cl] ~ dnorm(mu.k[i,k,cl], 1/exp(log.tau.k[k])^2)
                        mu.k[i,k,cl] <- inprod(ZG[i,], beta.k[k,]) ## no need for cl
                    }
                    log.sigma.i[i,cl] ~ dnorm(epsilon.i[i,cl], 1/exp(log.tau)^2)
                    epsilon.i[i,cl] <- inprod(ZG[i,], theta) ## no need for cl
                }
            }
            ## prior specifications
            for (t in 1:q) { # modifier parameters
                for (k in 1:p) { # focal parameters
                    beta.k[k,t] ~ dnorm(pr.cfs[k,t], pr.ses[k,t])
                }
                theta[t] ~ dnorm(pr.cfs[(p+1),t], pr.ses[(p+1),t])
            }
            for (k in 1:p) { # focal parameters
                log.tau.k[k] ~ dnorm(pr.tau[1,k], pr.tau[2,k])
            }
            log.tau ~ dnorm(pr.tau[1,(p+1)], pr.tau[2,(p+1)])
        }
    #    res <- jags.fit(datk, c("beta.k","theta","log.tau.k","log.tau"), 
    #        hsarx.lmm, inits=NULL, n.adapt=2000, n.update=3000, n.iter=1000)
        if (length(n.clones) == 1) {
            datk <- dclone(dat, n.clones, unchanged=c("X","ZG","G","n","m","p","q",
                "pr.cfs","pr.ses","pr.tau"), multiply="ncl")
            res <- if (is.null(cl)) {
                jags.fit(datk, c("beta.k","theta","log.tau.k","log.tau"), 
                    hsarx.lmm, inits=NULL, ...)
                } else {
                jags.parfit(cl, datk, c("beta.k","theta","log.tau.k","log.tau"), 
                    hsarx.lmm, inits=NULL, ...)
            }
        } else {
            res <- if (is.null(cl)) {
                dc.fit(dat, c("beta.k","theta","log.tau.k","log.tau"), 
                    hsarx.lmm, inits=NULL, n.clones=n.clones,
                    unchanged=c("X","ZG","G","n","m","p","q","pr.cfs","pr.ses","pr.tau"), multiply="ncl",
                    ...)
                } else {
                dc.parfit(cl, dat, c("beta.k","theta","log.tau.k","log.tau"), 
                    hsarx.lmm, inits=NULL, n.clones=n.clones,
                    unchanged=c("X","ZG","G","n","m","p","q","pr.cfs","pr.ses","pr.tau"), multiply="ncl",
                    ...)
                }
        }
    } else {
        ## SAR/SARX estimation
        sarx.lm <- function() {
            for (j in 1:m) {
                   logY[j] ~ dnorm(mu[j], 1/exp(log.sigma)^2)
                    mu[j] <- inprod(X[j,], beta)
            }
            for (k in 1:p) {
                beta[k] ~ dnorm(pr[k], pr2)
            }
            log.sigma ~ dnorm(pr[(p+1)], pr2)
        }
        dat <- list(logY=Y, X=X, m=m, p=p,
            pr=c(coef(lmmods[[1]]), log(summary(lmmods[[1]])$sigma)), pr2=0.01)
        if (length(n.clones) == 1) {
            datk <- dclone(dat, n.clones, unchanged=c("p", "pr","pr2"), multiply="m")
            res <- if (is.null(cl)) {
                    jags.fit(datk, c("beta","log.sigma"), sarx.lm, inits=NULL, ...)
                } else {
                    jags.parfit(cl, datk, c("beta","log.sigma"), sarx.lm,  inits=NULL, ...)
                }
        } else {
            res <- if (is.null(cl)) {
                    dc.fit(dat, c("beta","log.sigma"), sarx.lm, inits=NULL,
                        n.clones=n.clones, unchanged=c("p", "pr","pr2"), multiply="m", ...)
                } else {
                    dc.parfit(cl, dat, c("beta","log.sigma"), sarx.lm, inits=NULL,
                        n.clones=n.clones, unchanged=c("p", "pr","pr2"), multiply="m", ...)
                }
        }
    }
    res
}

library(Formula)
library(dclone)
library(sardata)
data(sardata01)
DAT <- data.frame(sardata01$sar, sardata01$study[match(sardata01$sar$study, rownames(sardata01$study)),])
#x <- hsarx(log(S+0.5) ~ log(A) | abs(latitude) | study, DAT)
cl <- makeSOCKcluster(3)
x <- hsarx(log(S+0.5) ~ log(A) | (taxon.group + island.type + abs(latitude) + I(log(extent)))^2 | study, DAT, 
    n.clones=5, cl=cl, n.adapt=2000, n.update=3000, n.iter=1000)
stopCluster(cl)

DATS <- DAT[1:191,]
DATS[] <- lapply(DATS, function(z) z[drop=TRUE])
x <- hsarx(log(S+0.5) ~ log(A) | taxon.group + island.type + abs(latitude) + I(log(extent)) | study, DATS, 
    n.clones=5, cl=cl, n.adapt=2000, n.update=3000, n.iter=1000, stage=0)


A=c(1:10, 10:1) / 10
d <- data.frame(S=rnorm(20, 1.9+0.25*A, 0.2), A, study=rep(c("b","a"), each=10))
db <- data.frame(study=c("a","b"), H=c(100,200))
dat <- data.frame(d, db[match(d$study, db$study),])

## SAR
x <- hsarx(S ~ A, dat)
## SARX
x <- hsarx(S ~ A * H, dat)
## HSAR
x <- hsarx(S ~ A | 1 | study, dat)
## HSARX
x <- hsarx(S ~ A | H | study, dat)
x <- hsarx(S ~ A * H | 1 | study, dat)
x <- hsarx(S ~ A * H | H | study, dat)
x$G

hsarx.fit(Y, X, Z, G, n.clones=2, cl=NULL)
##

library(dclone)
library(sardata)
setwd("c:/p/sar2")
data(sardata01)
load("c:/svn/dcr/pkg/sharx/data/sardata01.rda")

DAT <- data.frame(sardata01$sar, sardata01$study[match(sardata01$sar$study, rownames(sardata01$study)),])
#x <- hsarx(log(S+0.5) ~ log(A) | abs(latitude) | study, DAT)
cl <- makeSOCKcluster(3)
x <- hsarx(log(S+0.5) ~ log(A) | (taxon.group + island.type + abs(latitude) + I(log(extent)))^2 | study, DAT, n.clones=5, cl=cl)



#x <- hsarx(log(S+0.5) ~ log(A), DAT, subset=DAT$study=="hice2002", n.clones=1:3)
Y <- x$Y
X <- x$X
Z <- x$Z
G <- x$G



## make necessary data objects
m <- length(Y) # no. of islands
if (is.null(Z)) {
    n <- 1
    G <- rep(1, n)
} else {
    n <- length(unique(G)) # no. of studies
}
p <- ncol(X) # no. of focal parameters
dy <- lapply(1:n, function(i) Y[G == unique(G)[i]])
dx <- lapply(1:n, function(i) X[G == unique(G)[i],])
lmmods <- lapply(1:n, function(i) lm(dy[[i]] ~ dx[[i]]-1))
if (!is.null(Z)) {
    ## HSAR/HSARX estimation
    q <- ncol(Z) # no. of modifiers
    dz <- sapply(1:n, function(i) data.matrix(Z[G == unique(G)[i],])[1,])
    dz <- if (q > 1)
        t(dz) else data.matrix(dz)
    ## weighted averaging meta analysis for priors
    lmmods <- lapply(1:n, function(i) lm(dy[[i]] ~ dx[[i]]-1))
    cfs <- t(sapply(lmmods, coef))
    ses <- t(sapply(lmmods, function(z) coef(summary(z))[,2]))
    lsig <- sapply(lmmods, function(z) log(summary(z)$sigma))
    tau2 <- sapply(1:p, function(i) {
        vwts <- 1/ses[,i]^2
        fixedsumm <- sum(vwts * cfs[,i]) / sum(vwts)
        Q <- sum(((cfs[,i] - fixedsumm)^2) / ses[,i]^2)
        tau2 <- max(0, (Q - n - 1)/(sum(vwts) - sum(vwts^2)/sum(vwts)))
    })
    w <- sapply(1:p, function(i) 1/(tau2[i] + ses[,i]^2))
    wm <- lapply(1:p, function(i) lm(cfs[,i] ~ dz-1, weights=w[,i]))
    wmsig <- lm(lsig ~ dz-1)
    wm[[(p+1)]] <- wmsig
    ## create objects for priors
    pr.cfs <- t(sapply(wm, coef))
    pr.ses <- t(sapply(wm, function(z) 1/(coef(summary(z))[,2]^2)))
    pr.tau <- rbind(c(log(sqrt(tau2)), 0), rep(0.1, p+1))
    ZG <- Z[sapply(1:n, function(i) min(which(G == unique(G)[i]))),]
    dat <- list(logY=dcdim(data.matrix(Y)), X=X, ZG=ZG, G=G,
        n=n, m=m, p=p, q=q, ncl=1, 
        pr.cfs=pr.cfs, pr.ses=pr.ses, pr.tau=pr.tau)
    dimnames(dat$logY) <- NULL


    ## DC comes here
    hsarx.lmm <- function() {
        for (cl in 1:ncl) { # clones
            for (j in 1:m) { # islands
                ## focal model
                logY[j,cl] ~ dnorm(mu[j,cl], 1/exp(log.sigma.i[G[j],cl])^2)
                mu[j,cl] <- inprod(X[j,], beta.i[G[j],,cl])
            }
            for (i in 1:n) { # studies
                for (k in 1:p) { # focal parameters
                    ## modifier models for each focal parameter k
                    beta.i[i,k,cl] ~ dnorm(mu.k[i,k,cl], 1/exp(log.tau.k[k])^2)
                    mu.k[i,k,cl] <- inprod(ZG[i,], beta.k[k,]) ## no need for cl
                }
                log.sigma.i[i,cl] ~ dnorm(epsilon.i[i,cl], 1/exp(log.tau)^2)
                epsilon.i[i,cl] <- inprod(ZG[i,], theta) ## no need for cl
            }
        }
        ## prior specifications
        for (t in 1:q) { # modifier parameters
            for (k in 1:p) { # focal parameters
                beta.k[k,t] ~ dnorm(pr.cfs[k,t], pr.ses[k,t])
            }
            theta[t] ~ dnorm(pr.cfs[(p+1),t], pr.ses[(p+1),t])
        }
        for (k in 1:p) { # focal parameters
            log.tau.k[k] ~ dnorm(pr.tau[1,k], pr.tau[2,k])
        }
        log.tau ~ dnorm(pr.tau[1,(p+1)], pr.tau[2,(p+1)])
    }
#    res <- jags.fit(datk, c("beta.k","theta","log.tau.k","log.tau"), 
#        hsarx.lmm, inits=NULL, n.adapt=2000, n.update=3000, n.iter=1000)
    if (length(n.clones) == 1) {
        datk <- dclone(dat, n.clones, unchanged=c("X","ZG","G","n","m","p","q",
            "pr.cfs","pr.ses","pr.tau"), multiply="ncl")
        res <- if (is.null(cl)) {
            jags.fit(datk, c("beta.k","theta","log.tau.k","log.tau"), 
                hsarx.lmm, inits=NULL, n.adapt=2000, n.update=3000, n.iter=1000)
            } else {
            jags.parfit(cl, datk, c("beta.k","theta","log.tau.k","log.tau"), 
                hsarx.lmm, inits=NULL, n.adapt=2000, n.update=3000, n.iter=1000)
            }
        }
    } else {
        res <- if (is.null(cl)) {
            dc.fit(datk, c("beta.k","theta","log.tau.k","log.tau"), 
                hsarx.lmm, inits=NULL, n.clones=n.clones, 
                unchanged=c("X","ZG","G","n","m","p","q","pr.cfs","pr.ses","pr.tau"), multiply="ncl",
                n.adapt=2000, n.update=3000, n.iter=1000)
            } else {
            dc.parfit(datk, c("beta.k","theta","log.tau.k","log.tau"), 
                hsarx.lmm, inits=NULL, n.clones=n.clones, 
                unchanged=c("X","ZG","G","n","m","p","q","pr.cfs","pr.ses","pr.tau"), multiply="ncl",
                n.adapt=2000, n.update=3000, n.iter=1000)
            }
        }
    }

} else {
    ## SAR/SARX estimation
    sarx.lm <- function() {
        for (j in 1:m) {
               logY[j] ~ dnorm(mu[j], 1/exp(log.sigma)^2)
                mu[j] <- inprod(X[j,], beta)
        }
        for (k in 1:p) {
            beta[k] ~ dnorm(pr[k], pr2)
        }
        log.sigma ~ dnorm(pr[(p+1)], pr2)
    }
    dat <- list(logY=Y, X=X, m=m, p=p,
        pr=c(coef(lmmods[[1]]), log(summary(lmmods[[1]])$sigma)), pr2=0.01)
    datk <- dclone(dat, n.clones, unchanged=c("p", "pr","pr2"), multiply="m")
    res <- if (is.null(cl)) {
            jags.fit(datk, c("beta","log.sigma"), sarx.lm, n.adapt=2000, n.update=3000, n.iter=1000)
        } else {
            jags.parfit(cl, datk, c("beta","log.sigma"), sarx.lm, n.adapt=2000, n.update=3000, n.iter=1000)
        }
}



## data cloning pre-processing
## ZZ must have n rows and q columns ranked according to G
ZG <- Z[sapply(1:n, function(i) min(which(G == unique(G)[i]))),]
dat <- list(logY=dcdim(data.matrix(Y)), X=X, ZG=ZG, G=G,
    n=n, m=m, p=p, q=q, ncl=1, 
    ## prior must reflect model, check
    pr=rbind(coef(sslm0), coef(sslm1), coef(SIG)), pr2=0.01,
    pr1=c(log(sqrt(met0$tau2)), log(sqrt(met1$tau2)), mean(sapply(lmmods, function(z) log(summary(z)$sigma)))))
datk <- dclone(dat, n.clones, unchanged=c("X","ZG","G","n","m","p","q","pr","pr1","pr2"), multiply="ncl")
res <- if (is.null(cl)) {
    jags.fit(datk, c("beta0","beta1","theta","log.tau","log.tau0","log.tau1"), 
        hsarx.lmm, inits=NULL, n.adapt=2000, n.update=3000, n.iter=1000)
    } else {
    jags.parfit(cl, datk, c("beta0","beta1","theta","log.tau","log.tau0","log.tau1"), 
        hsarx.lmm, inits=NULL, n.adapt=2000, n.update=3000, n.iter=1000)
    }
}


hsarx.lmm <- function() { ## this is for cloning
    for (j in 1:m) {
        for (i in id[j,1]:id[j,2]){
            for (cl in 1:ncl) {
                logY[i,cl] ~ dnorm(mu[i,cl], 1/exp(log.sigma[j,cl])^2)
                mu[i,cl] <- inprod(X[i,,cl], beta[j,,cl])
            }
        }
        for (cl in 1:ncl) {
            mu0[j,cl] <- inprod(Z[j,], beta0[1,])
            mu1[j,cl] <- inprod(Z[j,], beta1[1,])
            nu[j,cl] <- inprod(Z[j,], theta[1,])
            beta[j,1,cl] ~ dnorm(mu0[j,cl], 1/exp(log.tau0)^2)
            beta[j,2,cl] ~ dnorm(mu1[j,cl], 1/exp(log.tau1)^2)
            log.sigma[j,cl] ~ dnorm(nu[j,cl], 1/exp(log.tau)^2)
        }
    }
    for (k in 1:p) {
        beta0[1,k] ~ dnorm(pr[k], pr2)
        beta1[1,k] ~ dnorm(pr[k+p], pr2)
        theta[1,k] ~ dnorm(pr[k+(2*p)], pr2)
    }
    log.tau0 ~ dnorm(pr1[1], pr2)
    log.tau1 ~ dnorm(pr1[2], pr2)
    log.tau ~ dnorm(pr1[3], pr2)
}



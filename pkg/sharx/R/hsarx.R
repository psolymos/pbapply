#http://cran.r-project.org/web/packages/Formula/vignettes/Formula.pdf
hsarx <- 
function(formula, data, group, n.clones=1, cl=NULL, subset, na.action, ...)
{
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
    out <- hsarx.fit(Y, X, Z, G, n.clones, cl, ...)
    class(out) <- "hsar"
    out$formula <- f
    out$model <- mf
    out
}

hsarx.fit <- 
function(Y, X, Z, G, n.clones=1, cl=NULL, ...)
{
    list(Y=Y, X=X, Z=Z, G=G, n.clones=n.clones, cl=cl)
}

library(Formula)
d <- data.frame(S=c(1:10, 10:1), A=c(1:10, 10:1) / 10, study=rep(c("b","a"), each=10))
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

##

library(dclone)
library(sardata)
setwd("c:/p/sar2")
data(sardata01)

DAT <- data.frame(sardata01$sar, sardata01$study[match(sardata01$sar$study, rownames(sardata01$study)),])
x <- hsarx(log(S+0.5) ~ log(A) | abs(latitude) | study, DAT)

#x <- hsarx(log(S+0.5) ~ log(A), DAT, subset=dat$study=="hice2002")
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
    dz <- t(sapply(1:n, function(i) Z[G == unique(G)[i],][1,]))
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
    
    ## DC comes here
    
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
dat <- list(logY=dcdim(data.matrix(Y)), X=dcdim(array(X,dim=c(dim(X), 1))), Z=Z,
    p=ncol(Z), m=nrow(Z), ncl=1, pr=c(coef(sslm0), coef(sslm1), coef(SIG)), pr2=0.01,
    pr1=c(log(sqrt(met0$tau2)), log(sqrt(met1$tau2)), mean(sapply(lmmods, function(z) log(summary(z)$sigma)))))
datk <- dclone(dat, k[i], unchanged=c("Z","id","p","m","pr","pr1","pr2"), multiply="ncl")
res <- if (is.null(cl)) {
    jags.fit(datk, c("beta0","beta1","theta","log.tau","log.tau0","log.tau1"), 
        hsarx.lmm, inits=NULL, n.adapt=2000, n.update=3000, n.iter=1000)
    } else {
    jags.parfit(cl, datk, c("beta0","beta1","theta","log.tau","log.tau0","log.tau1"), 
        hsarx.lmm, inits=NULL, n.adapt=2000, n.update=3000, n.iter=1000)
    }
}

stopCluster(cl)

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



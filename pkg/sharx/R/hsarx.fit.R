hsarx.fit <- 
function(Y, X, Z, G, n.clones, cl=NULL, ...)
{
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
#        lmmods <- lapply(1:n, function(i) lm(dy[[i]] ~ dx[[i]]-1))
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
        if (q == 1) {
            pr.cfs <- matrix(pr.cfs, ncol=1)
            pr.ses <- matrix(pr.ses, ncol=1)
        }
        dim(pr.ses) <- dim(pr.cfs)
        pr.tau <- rbind(c(log(sqrt(tau2)), 0), rep(0.1, p+1))
        ZG <- Z[sapply(1:n, function(i) min(which(G == unique(G)[i]))),]
        ZG <- data.matrix(ZG)
        dat <- list(logY=dcdim(data.matrix(Y)), X=X, ZG=ZG, G=G,
            n=n, m=m, p=p, q=q, ncl=1, 
            pr.cfs=pr.cfs, pr.ses=pr.ses, pr.tau=pr.tau)
#        dimnames(dat$logY) <- NULL
        ## DC comes here
#        hsarx.lmm <- function() {
#            for (cl in 1:ncl) { # clones
#                for (j in 1:m) { # islands # focal model
#                    logY[j,cl] ~ dnorm(mu[j,cl], 1/exp(log.sigma.i[G[j],cl])^2)
#                    mu[j,cl] <- inprod(X[j,], beta.i[G[j],,cl])
#                }
#                for (i in 1:n) { # studies
#                    for (k in 1:p) { # focal parameters # modifier models for each focal parameter k
#                        beta.i[i,k,cl] ~ dnorm(mu.k[i,k], 1/exp(log.tau.k[k])^2)
#                    }
#                    log.sigma.i[i,cl] ~ dnorm(epsilon.i[i], 1/exp(log.tau)^2)
#                }
#            }
#            for (i in 1:n) {
#                for (k in 1:p) {
#                    mu.k[i,k] <- inprod(ZG[i,], beta.k[k,])
#                }
#                epsilon.i[i] <- inprod(ZG[i,], theta)
#            } # prior specifications
#            for (t in 1:q) { # modifier parameters
#                for (k in 1:p) { # focal parameters
#                    beta.k[k,t] ~ dnorm(pr.cfs[k,t], pr.ses[k,t])
#                }
#                theta[t] ~ dnorm(pr.cfs[(p+1),t], pr.ses[(p+1),t])
#            }
#            for (k in 1:p) { # focal parameters
#                log.tau.k[k] ~ dnorm(pr.tau[1,k], pr.tau[2,k])
#            }
#            log.tau ~ dnorm(pr.tau[1,(p+1)], pr.tau[2,(p+1)])
#        }
        hsarx.lmm <- structure(c(
            c("model {", 
            "            for (cl in 1:ncl) { # clones", 
            "                for (j in 1:m) { # islands # focal model", 
            "                    logY[j,cl] ~ dnorm(mu[j,cl], 1/exp(log.sigma.i[G[j],cl])^2)", 
            "                    mu[j,cl] <- inprod(X[j,], beta.i[G[j],,cl])", 
            "                }", 
            "                for (i in 1:n) { # studies", 
            "                    for (k in 1:p) { # focal parameters # modifier models for each focal parameter k", 
            "                        beta.i[i,k,cl] ~ dnorm(mu.k[i,k], 1/exp(log.tau.k[k])^2)", 
            "                    }", 
            "                    log.sigma.i[i,cl] ~ dnorm(epsilon.i[i], 1/exp(log.tau)^2)", 
            "                }", 
            "            }", 
            "            for (i in 1:n) {", 
            "                for (k in 1:p) {", 
            "                    mu.k[i,k] <- inprod(ZG[i,], beta.k[k,])", 
            "                }", 
            "                epsilon.i[i] <- inprod(ZG[i,], theta)", 
            "            } # prior specifications", 
            "            for (t in 1:q) { # modifier parameters", 
            "                for (k in 1:p) { # focal parameters", 
            "                    beta.k[k,t] ~ dnorm(pr.cfs[k,t], pr.ses[k,t])", 
            "                }", 
            "                theta[t] ~ dnorm(pr.cfs[(p+1),t], pr.ses[(p+1),t])", 
            "            }", 
            "            for (k in 1:p) { # focal parameters", 
            "                log.tau.k[k] ~ dnorm(pr.tau[1,k], pr.tau[2,k])", 
            "            }", 
            "            log.tau ~ dnorm(pr.tau[1,(p+1)], pr.tau[2,(p+1)])", 
            "        }")
            ), class = "custommodel")
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
#        sarx.lm <- function() {
#            for (j in 1:m) {
#                   logY[j] ~ dnorm(mu[j], 1/exp(log.sigma)^2)
#                    mu[j] <- inprod(X[j,], beta)
#            }
#            for (k in 1:p) {
#                beta[k] ~ dnorm(pr[k], pr2)
#            }
#            log.sigma ~ dnorm(pr[(p+1)], pr2)
#        }
        sarx.lm <- structure(c(
            "model {",
            "            for (j in 1:m) {",
            "                   logY[j] ~ dnorm(mu[j], 1/exp(log.sigma)^2)",
            "                    mu[j] <- inprod(X[j,], beta)",
            "            }",
            "            for (k in 1:p) {",
            "                beta[k] ~ dnorm(pr[k], pr2)",
            "            }",
            "            log.sigma ~ dnorm(pr[(p+1)], pr2)",
            "        }"
            ), class = "custommodel")
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

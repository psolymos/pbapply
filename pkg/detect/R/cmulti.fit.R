cmulti.fit <-
function(Y, D, X=NULL, type=c("rem", "mix", "dis"), 
inits=NULL, method="Nelder-Mead", ...)
{
    Ysum <- rowSums(Y, na.rm=TRUE)
    Y <- Y[Ysum > 0,,drop=FALSE]
    D <- D[Ysum > 0,,drop=FALSE]
    if (!is.null(X))
        X <- X[Ysum > 0,,drop=FALSE]
    Ysum <- Ysum[Ysum > 0]

    type <- match.arg(type)
    pifun <- switch(type,
            "dis" = function(r, tau) 1-exp(-(r/tau)^2),
            "rem"  = function(t, phi) 1-exp(-t*phi),
            "mix"  = function(t, phi, c) 1-c*exp(-t*phi))
    Yok <- !is.na(Y)
    n <- nrow(Y)
    k <- ncol(Y)
    if (is.null(inits))
        v0 <- detect:::cmulti.fit0(Y, D, type)$coef
    nlimit <- c(.Machine$double.xmin, .Machine$double.xmax)^(1/3)
    ## parameter is fixed, removal mixture
    if (is.null(X) && type == "mix") {
        nll.fun1 <- function(pv, cv) {
            CP <- pifun(D, poisson("log")$linkinv(pv),
                binomial("logit")$linkinv(cv))
            P <- CP - cbind(0, CP[, -k, drop=FALSE])
            Psum <- rowSums(P, na.rm=TRUE)
            PPsum <- P / Psum
            nll <- -sum(sapply(1:n, function(i) {
                logdmultinom(Y[i,Yok[i,]], Ysum[i], PPsum[i,Yok[i,]])
            }))
            if (nll %in% c(NA, NaN, Inf, -Inf))
                nlimit[2] else nll
        }
        if (is.null(inits))
            inits <- v0
        require(stats4)
        res <- suppressWarnings(stats4:::mle(nll.fun1, 
            list(pv=inits[1], cv=inits[2]), 
            method=method, ...))
        rval <- list(coef=res@coef, 
            vcov=res@vcov, 
            loglik=-res@details$value)
    }
    ## parameter is not fixed, removal mixture
    if (!is.null(X) && type == "mix") {
        nll.fun2 <- function(param) {
            CP <- pifun(D, poisson("log")$linkinv(param[1]), 
                binomial("logit")$linkinv(drop(X %*% param[-1])))
            P <- CP - cbind(0, CP[, -k, drop=FALSE])
            Psum <- rowSums(P, na.rm=TRUE)
            PPsum <- P / Psum
            nll <- -sum(sapply(1:n, function(i) {
                logdmultinom(Y[i,Yok[i,]], Ysum[i], PPsum[i,Yok[i,]])
            }))
            if (nll %in% c(NA, NaN, Inf, -Inf))
                nlimit[2] else nll
        }
        if (is.null(inits)) {
            inits <- rep(0, ncol(X)+1)
            inits[1:2] <- v0
        }
        res <- optim(inits, nll.fun2, method=method, hessian=TRUE, ...)
        rval <- list(coef=res$par, 
            vcov=try(solve(res$hessian)), 
            loglik=-res$value)
    }
    ## parameter is fixed, rem or dist
    if (is.null(X) && type != "mix") {
        nll.fun3 <- function(pv) {
            CP <- pifun(D, poisson("log")$linkinv(pv))
            P <- CP - cbind(0, CP[, -k, drop=FALSE])
            Psum <- rowSums(P, na.rm=TRUE)
            PPsum <- P / Psum
            nll <- -sum(sapply(1:n, function(i) {
                logdmultinom(Y[i,Yok[i,]], Ysum[i], PPsum[i,Yok[i,]])
            }))
            if (nll %in% c(NA, NaN, Inf, -Inf))
                nlimit[2] else nll
        }
        if (is.null(inits))
            inits <- v0
        require(stats4)
        res <- suppressWarnings(stats4:::mle(nll.fun3, list(pv=inits), 
            method=method, ...))
        rval <- list(coef=res@coef, 
            vcov=res@vcov, 
            loglik=-res@details$value)
    } 
    ## parameter is not fixed, rem or dist
    if (!is.null(X) && type != "mix") {
        nll.fun4 <- function(param) {
            CP <- pifun(D, poisson("log")$linkinv(drop(X %*% param)))
            P <- CP - cbind(0, CP[, -k, drop=FALSE])
            Psum <- rowSums(P, na.rm=TRUE)
            PPsum <- P / Psum
            nll <- -sum(sapply(1:n, function(i) {
                logdmultinom(Y[i,Yok[i,]], Ysum[i], PPsum[i,Yok[i,]])
            }))
            if (nll %in% c(NA, NaN, Inf, -Inf))
                nlimit[2] else nll
        }
        if (is.null(inits)) {
            inits <- rep(0, ncol(X))
            inits[1] <- v0
        }
        res <- optim(inits, nll.fun4, method=method, hessian=TRUE, ...)
        rval <- list(coef=res$par, 
            vcov=try(solve(res$hessian)), 
            loglik=-res$value)
    }
    if (inherits(rval$vcov, "try-error"))
        rval$vcov <- matrix(NA, length(rval$coef), length(rval$coef))
    rval$coefficients <- unname(rval$coef)
    rval$coef <- NULL
    rval$vcov <- unname(rval$vcov)
    rval
}

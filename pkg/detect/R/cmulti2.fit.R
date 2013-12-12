cmulti2.fit <-
function(Y, D1, D2, X1=NULL, X2=NULL, 
inits=NULL, method="Nelder-Mead", ...)
{
    ## flattening the array
    Flat <- function(x, DIM, dur=TRUE) {
        x <- array(x, DIM)
        if (!dur) {
            x <- aperm(x,c(1,3,2))
        }
        dim(x) <- c(DIM[1], DIM[2]*DIM[3])
        x
    }
    if (length(dim(Y)) != 3)
        stop("Y must have 3 dimensions")
    n <- dim(Y)[1]
    k1 <- dim(Y)[2]
    k2 <- dim(Y)[3]
    Y1 <- t(sapply(1:n, function(i) {
        count <- rowSums(Y[i,,], na.rm=TRUE)
        nas <- rowSums(is.na(Y[i,,]))
        count[nas == k2] <- NA
        count
    }))
    Y2 <- t(sapply(1:n, function(i) {
        count <- colSums(Y[i,,], na.rm=TRUE)
        nas <- colSums(is.na(Y[i,,]))
        count[nas == k2] <- NA
        count
    }))
    Yok <- !is.na(Y)
    if (!all(!is.na(D1) == !is.na(Y1)))
        stop("NA pattern mismatch for Y and D1")
    if (!all(!is.na(D2) == !is.na(Y2)))
        stop("NA pattern mismatch for Y and D2")
    Yflat <- array(Y, c(dim(Y)[1], dim(Y)[2]*dim(Y)[3]))
    YokFlat <- array(Yok, c(dim(Y)[1], dim(Y)[2]*dim(Y)[3]))
    Ysum <- rowSums(Yflat, na.rm=TRUE)
    np1 <- if (is.null(X1))
        1 else ncol(X1)
    np2 <- if (is.null(X2))
        1 else ncol(X2)
    if (is.null(inits)) {
        inits1 <- NULL
        inits2 <- NULL
    } else {
        inits1 <- inits[1:np1]
        inits1 <- inits[(np1+1):(np1+np2)]
    }
    ## finest starting values
    est1 <- cmulti.fit(Y1, D1, X1, "rem", inits1, method, ...)
    est2 <- cmulti.fit(Y2, D2, X2, "dis", inits2, method, ...)
    inits <- c(est1$coefficients, est2$coefficients)
    if (is.null(X1))
        X1 <- matrix(1, dim(Y)[1], 1)
    if (is.null(X2))
        X2 <- matrix(1, dim(Y)[1], 1)
    pifun1 <- function(t, phi) 1-exp(-t*phi)
    pifun2 <- function(r, tau) 1-exp(-(r/tau)^2)
    nlimit <- c(.Machine$double.xmin, .Machine$double.xmax)^(1/3)
    Yflat <- Yflat[Ysum > 0,,drop=FALSE]
    YokFlat <- YokFlat[Ysum > 0,,drop=FALSE]
    D1 <- D1[Ysum > 0,,drop=FALSE]
    D2 <- D2[Ysum > 0,,drop=FALSE]
    X1 <- X1[Ysum > 0,,drop=FALSE]
    X2 <- X2[Ysum > 0,,drop=FALSE]
    Ysum <- Ysum[Ysum > 0]
    n <- length(Ysum)
    DIM <- c(n, k1, k2)
    ## negative log-likelihood fn
    nll.fun <- function(param) {
        CP1 <- pifun1(D1, 
            poisson("log")$linkinv(drop(X1 %*% param[1:np1])))
        P1 <- CP1 - cbind(0, CP1[, -k1, drop=FALSE])
        Psum1 <- rowSums(P1, na.rm=TRUE)

        CP2 <- pifun2(D2, 
            poisson("log")$linkinv(drop(X2 %*% param[(np1+1):(np1+np2)])))
        P2 <- CP2 - cbind(0, CP2[, -k2, drop=FALSE])
        Psum2 <- rowSums(P2, na.rm=TRUE)

        Pflat <- Flat(P1, DIM, dur=TRUE) * Flat(P2, DIM, dur=FALSE)
        PsumFlat <- Psum1 * Psum2
        PPsumFlat <- Pflat / PsumFlat
        nll <- -sum(sapply(1:n, function(i) {
            logdmultinom(Yflat[i,YokFlat[i,]], Ysum[i], PPsumFlat[i,YokFlat[i,]])
        }))
        if (nll %in% c(NA, NaN, Inf, -Inf))
            nlimit[2] else nll
    }
    ## estimation
    res <- optim(inits, nll.fun, method=method, hessian=TRUE, ...)
    rval <- list(coef=res$par, 
        vcov=try(solve(res$hessian)), 
        loglik=-res$value)
    ## assembling return value
    if (inherits(rval$vcov, "try-error"))
        rval$vcov <- matrix(NA, length(rval$coef), length(rval$coef))
    rval$coefficients <- unname(rval$coef)
    rval$coef <- NULL
    rval$vcov <- unname(rval$vcov)
    rval
}

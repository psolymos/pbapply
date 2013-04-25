## constant conditional multinomial model
## collapsing counts by methodology
cmulti.fit0 <-
function(Y, D, type=c("rem", "mix", "dis"), interval=c(-25, 25), ...)
{
    type <- match.arg(type)
    pifun <- switch(type,
            "dis" = function(r, tau) 1-exp(-(r/tau)^2),
            "rem"  = function(t, phi) 1-exp(-t*phi),
            "mix"  = function(t, phi, c) 1-c*exp(-t*phi))
    nll.fun <- if (type == "mix")
        function(param) {
            if (length(param) < 2)
                param <- c(param, 1)
            CP <- pifun(D0, poisson("log")$linkinv(param[1]),
                binomial("logit")$linkinv(param[2]))
            P <- CP - cbind(0, CP[, -k, drop=FALSE])
            Psum <- rowSums(P, na.rm=TRUE)
            PPsum <- P / Psum
            nll <- -sum(sapply(1:n, function(i) {
                logdmultinom(Y0[i,Y0ok[i,]], Y0sum[i], PPsum[i,Y0ok[i,]])
            }))
            if (nll %in% c(NA, NaN, Inf, -Inf))
                nlimit[2] else nll
        } else function(param) {
            CP <- pifun(D0, poisson("log")$linkinv(param))
            P <- CP - cbind(0, CP[, -k, drop=FALSE])
            Psum <- rowSums(P, na.rm=TRUE)
            PPsum <- P / Psum
            nll <- -sum(sapply(1:n, function(i) {
                logdmultinom(Y0[i,Y0ok[i,]], Y0sum[i], PPsum[i,Y0ok[i,]])
            }))
            if (nll %in% c(NA, NaN, Inf, -Inf))
                nlimit[2] else nll
        }
    nlimit <- c(.Machine$double.xmin, .Machine$double.xmax)^(1/3)
    tmp <- bymethod(Y, D)
    Y0 <- tmp$Y
    D0 <- tmp$D
    Y0sum <- rowSums(Y0, na.rm=TRUE)

    Y0 <- Y0[Y0sum > 0,,drop=FALSE]
    D0 <- D0[Y0sum > 0,,drop=FALSE]
    Y0sum <- Y0sum[Y0sum > 0]

    Y0ok <- !is.na(Y0)
    n <- nrow(Y0)
    k <- ncol(Y0)
    if (type != "mix") {
        out <- optimize(nll.fun, interval=interval, ...)
        rval <- list(coef=out$minimum, 
            loglik=-out$objective)
    } else {
        out <- optimize(nll.fun, interval=interval, ...)
        out <- optim(c(out$minimum, 0), nll.fun, hessian=FALSE, ...)
        rval <- list(coef=out$par, 
            loglik=-out$value)
    }
    rval
}

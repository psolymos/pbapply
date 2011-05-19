vcov.rsf <-
function (object, type, ...)
{
    boot <- object$bootstrap
    if (missing(type)) {
        type <- if (is.null(boot))
        "mle" else "boot"
    }
    type <- match.arg(type, c("mle", "boot"))
    if (type == "boot" && is.null(boot))
        stop("no bootstrap results found")
    np <- object$np
    if (type == "boot") {
        rval <- cov(t(boot))
    } else {
        rval <- matrix(NA, np, np)
        h <- if (object$link == "log") {
            data.matrix(object$results$hessian[-1,-1])
        } else {
            object$results$hessian
        }
        rval[1:np, 1:np] <- solve(h)
    }
    rval <- data.matrix(rval)
    cf <- coef(object)
    colnames(rval) <- rownames(rval) <- names(cf)
    return(rval)
}


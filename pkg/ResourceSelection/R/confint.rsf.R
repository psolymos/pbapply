confint.rsf <-
function (object, parm, level = 0.95, type, ...)
{
    boot <- object$bootstrap
    if (missing(type)) {
        type <- if (is.null(boot))
        "mle" else "boot"
    }
    type <- match.arg(type, c("mle", "boot"))
    if (type == "boot" && is.null(boot))
        stop("no bootstrap results found")
    if (type != "boot")
        boot <- NULL
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm)) {
        parm <- pnames
    } else {
        if (is.numeric(parm))
            parm <- pnames[parm]
    }
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%", sep="")
    ci <- array(NA, dim = c(length(parm), 2), dimnames = list(parm, pct))
    if (type == "mle") {
#        fac <- qt(a, object$df.residual)
        fac <- qnorm(a)
        ses <- sqrt(diag(vcov(object, type)))
        ci[] <- cf[parm] + ses[parm] %o% fac
    } else {
        cii <- t(apply(boot, 1, quantile, probs=a))
        rownames(cii) <- pnames
        ci[] <- cii[parm,,drop=FALSE]
    }
    return(ci)
}


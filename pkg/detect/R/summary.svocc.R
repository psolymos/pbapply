summary.svocc <-
function (object, type, ...) 
{
    boot <- extractBOOT(object)
    if (missing(type)) {
        type <- "mle"
        if (object$penalized && !is.null(boot))
            type <- "boot"
        if (object$penalized && is.null(boot))
            type <- "pmle"
    }
    type <- match.arg(type, c("mle", "pmle", "boot"))
    if (type == "pmle" && !object$penalized)
        stop("cannot provide PMLE type summary")
    if (type == "boot" && is.null(boot))
        stop("cannot provide Bootstrap type summary")
    if (type != "boot")
        boot <- NULL

    ksta <- length(object$coefficients$sta)
    kdet <- length(object$coefficients$det)

    se <- c(object$std.error$sta, object$std.error$det)
    coefs <- c(object$coefficients$sta, object$coefficients$det)
    if (type == "mle" && object$penalized) {
        tmp <- extractMLE(object)
        se <- tmp$std.error
        coefs <- tmp$coefficients
    }
    if (type == "boot") {
        tmp <- extractBOOT(object)
        se <- tmp$std.error
    }

    tstat <- coefs/se
#    pval <- 2 * pt(abs(tstat), object$df.residual, lower.tail = FALSE)
    ## z test because no overdspersion
    pval <- 2 * pnorm(-abs(tstat))
    coefs <- cbind(coefs, se, tstat, pval)
#    colnames(coefs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    colnames(coefs) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    sta <- coefs[1:ksta, , drop = FALSE]
    det <- coefs[(ksta + 1):(ksta + kdet), , drop = FALSE]
    out <- list(call = object$call, sta = sta, det = det, loglik = object$loglik, 
        converged = object$converged, penalized = type == "pmle", method = object$method, 
        link = object$link, control = object$control, n = object$nobs, 
        df.residual = object$df.residual, bootstrap=boot, type=type)
    class(out) <- "summary.svocc"
    return(out)
}


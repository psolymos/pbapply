summary.svabu <-
function (object, type, ...) 
{
    boot <- extractBOOT(object)
    if (missing(type)) {
        type <- if (is.null(boot))
        "cmle" else "boot"
    }
    type <- match.arg(type, c("cmle", "boot"))
    if (type == "boot" && is.null(boot))
        stop("cannot provide Bootstrap type summary")
    if (type != "boot")
        boot <- NULL

    ksta <- length(object$coefficients$sta)
    kdet <- length(object$coefficients$det)
    kzif <- ifelse(object$zeroinfl, length(object$coefficients$zif), 0)
    coefs <- coef(object)
    if (type == "boot") {
        se <- boot$std.error
    } else {
        se <- c(object$std.error$sta, object$std.error$det)
        if (object$zeroinfl) {
            se <- c(se, object$std.error$zif)
        }
    }
    tstat <- coefs/se
#    pval <- 2 * pt(abs(tstat), object$df.residual, lower.tail = FALSE)
    ## z test because no overdspersion
    pval <- 2 * pnorm(-abs(tstat))
    coefs <- cbind(coefs, se, tstat, pval)
#    colnames(coefs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    colnames(coefs) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    sta <- coefs[1:ksta, , drop = FALSE]
    rownames(sta) <- names(coef(object, "sta"))
    det <- coefs[(ksta + 1):(ksta + kdet), , drop = FALSE]
    rownames(det) <- names(coef(object, "det"))
    zif <- NULL
    if (object$zeroinfl) {
        zif <- coefs[(ksta + kdet + 1):(ksta + kdet + kzif), , drop = FALSE]
        rownames(zif) <- names(coef(object, "zif"))
    }

    out <- list(call = object$call, sta = sta, det = det, zif = zif, loglik = object$loglik, 
        converged = object$converged, zeroinfl = object$zeroinfl, 
        control = object$control, nobs = object$nobs, link = object$link, terms = object$terms,
        df.residual = object$df.residual, bootstrap=boot, type=type, area = object$area,
        distr=ifelse(inherits(object, "svabu_p"), "Poisson", "Negative Binomial"), var=object$var)
    class(out) <- "summary.svabu"
    return(out)
}


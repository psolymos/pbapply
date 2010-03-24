`summary.singleocc` <-
function (object, ...) 
{
    kocc <- length(object$coefficients$occ)
    kdet <- length(object$coefficients$det)
    se <- c(object$std.error$occ, object$std.error$det)
    coef <- c(object$coefficients$occ, object$coefficients$det)
    tstat <- coef/se
    pval <- 2 * pt(abs(tstat), object$df.residual, lower.tail = FALSE)
    coef <- cbind(coef, se, tstat, pval)
    colnames(coef) <- c("Estimate", "Std. Error", "t value", 
        "Pr(>|t|)")
    occ <- coef[1:kocc, , drop = FALSE]
    det <- coef[(kocc + 1):(kocc + kdet), , drop = FALSE]
    out <- list(call = object$call, occ = occ, det = det, loglik = object$loglik, 
        results = object$results, converged = object$converged, 
        penalized = object$penalized, method = object$method, 
        link = object$link, control = object$control, n = object$n, 
        df.residual = object$df.residual)
    class(out) <- "summary.singleocc"
    return(out)
}

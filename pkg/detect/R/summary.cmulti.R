summary.cmulti <-
function (object, ...) 
{
    k <- length(object$coefficients)
    coefs <- coef(object)
    se <- sqrt(diag(vcov(object)))
    tstat <- coefs/se
#    pval <- 2 * pt(abs(tstat), object$df.residual, lower.tail = FALSE)
    ## z test because no overdspersion
    pval <- 2 * pnorm(-abs(tstat))
    coefs <- cbind(coefs, se, tstat, pval)
#    colnames(coefs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    colnames(coefs) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    coefs <- coefs[1:k, , drop = FALSE]
    rownames(coefs) <- names(coef(object))
    out <- list(call = object$call, coefficients=coefs, loglik = object$loglik,
        fitted.values = object$fitted.values, bic=BIC(object),
        type=object$type)
    class(out) <- "summary.cmulti"
    return(out)
}

summary.dcglm <-
function(object, ...){
    COEF <- coef(object)
    SE <- object$std.error
    z <- COEF / SE
    p <-  2 * pnorm(-abs(z))
    stab <- cbind("Estimate" = COEF, "Std. Error" = SE,
        "z value" = z, "Pr(>|z|)" = p)
    rval <- list(call = object$call, 
        coefficients = stab, 
        loglik = object$loglik,
        df.residual = object$df.residual,
        df.null = object$df.null)
    class(rval) <- "summary.dcglm"
    rval
}


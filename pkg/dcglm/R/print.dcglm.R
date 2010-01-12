print.dcglm <-
function(x, digits = max(3, getOption("digits") - 3), ...) {
    cat("\nCall: ", deparse(x$call), "\n\n")
    cat("Coefficients:\n")
    print.default(format(x$coefficients, digits = digits), print.gap = 2, quote = FALSE)
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ", x$df.residual, "Residual\n")
    cat("Log Likelihood:\t   ", format(signif(x$loglik, digits)), "\n")
    invisible(x)
}


print.summary.dcglm <-
function (x, digits = max(3, getOption("digits") - 3), 
    signif.stars = getOption("show.signif.stars"), ...) 
{
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")
    cat("\nCoefficients:\n")
    printCoefmat(x$coefficients, digits = digits, signif.stars = signif.stars, na.print = "NA", ...)
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ", x$df.residual, "Residual\n")
    cat("Log Likelihood:\t   ", format(signif(x$loglik, digits)), "\n")
    invisible(x)
}


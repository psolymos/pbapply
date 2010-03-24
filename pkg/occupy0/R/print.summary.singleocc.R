`print.summary.singleocc` <-
function (x, digits = max(3, getOption("digits") - 3), ...) 
{
    cat("\nCall:", deparse(x$call, width.cutoff = floor(getOption("width") * 
        0.85)), "\n", sep = "\n")
    cat(paste("Single survey site-occupancy model", sep = ""))
    pen <- if (x$penalized)
        "Penalized " else ""
    cat(paste("\n", pen, "Maximum Likelihood estimates (", x$method, " method)\n\n", sep = ""))
    cat(paste("Occupancy model coefficients with ", x$link["occ"], " link:\n", sep = ""))
    printCoefmat(x$occ, digits = digits, signif.legend = FALSE)
    cat(paste("Detection model coefficients with ", x$link["det"], " link:\n", sep = ""))
    printCoefmat(x$det, digits = digits, signif.legend = FALSE)
    if (!any(is.na(c(array(x$occ), array(x$det))))) {
        if (getOption("show.signif.stars") & any(rbind(x$occ, x$det)[,4] < 0.1))
            cat("---\nSignif. codes: ", "0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", "\n")
    }
    cat("\nLog-likelihood:", formatC(x$loglik, digits = digits), 
        "on", x$n - x$df.residual, "Df\nAIC =",
        formatC(-2 * x$loglik + 2 * (x$n - x$df.residual), digits = digits), "\n\n")
#    if (!x$converged) 
#        cat("Warning:\n  Model did not converge\n\n")
    invisible(x)
}

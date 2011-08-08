print.summary.svocc <-
function (x, digits, ...) 
{
    if (missing(digits))
        digits <- max(3, getOption("digits") - 3)
    cat("\nCall:", deparse(x$call, width.cutoff = floor(getOption("width") * 
        0.85)), "\n", sep = "\n")
    cat(paste("Single visit site-occupancy model", sep = ""))
    pen <- if (x$penalized)
        "Penalized " else ""
    cat(paste("\n", pen, "Maximum Likelihood estimates (", x$method, " method)\n", sep = ""))
    if (!is.null(x$bootstrap)) {
        btype <- switch(x$bootstrap$type,
            "nonpar" = "Nonparametric",
            "param" = "Parametric")
        nboot <- x$bootstrap$B
        cat(paste("with ", btype, " Bootstrap standard errors (B = ", nboot, ")\n", sep = ""))
    }
    cat(paste("\nOccupancy model coefficients with ", x$link["sta"], " link:\n", sep = ""))
    printCoefmat(x$sta, digits = digits, signif.legend = FALSE)
    cat(paste("Detection model coefficients with ", x$link["det"], " link:\n", sep = ""))
    printCoefmat(x$det, digits = digits, signif.legend = FALSE)
    if (!any(is.na(c(array(x$sta), array(x$det))))) {
        if (getOption("show.signif.stars") & any(rbind(x$sta, x$det)[,4] < 0.1))
            cat("---\nSignif. codes: ", "0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", "\n")
    }
    cat("\nLog-likelihood:", formatC(x$loglik, digits = digits), 
        "on", x$nobs - x$df.residual, "Df\nAIC =",
        formatC(-2 * x$loglik + 2 * (x$nobs - x$df.residual), digits = digits), "\n")
    Conv <- if (!x$penalized)
        x$converged[1] else x$converged[2]
    if (!Conv) 
        cat("Warning:\n  Model did not converge\n\n")
    cat("\n")
    invisible(x)
}


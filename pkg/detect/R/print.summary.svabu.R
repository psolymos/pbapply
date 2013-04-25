print.summary.svabu <-
function (x, digits, ...) 
{
    statename <- if (all(x$area == 1))
        "abundance" else "density"
    if (missing(digits))
        digits <- max(3, getOption("digits") - 3)
    cat("\nCall:", deparse(x$call,
        width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")
    modt <- if (x$zeroinfl)
        "Zero Inflated " else ""
    cat(paste("Single visit Binomial - ", modt, x$distr, " model", sep = ""))
    cat(paste("\nConditional Maximum Likelihood estimates\n", sep = ""))
    if (!is.null(x$bootstrap)) {
        btype <- switch(x$bootstrap$type,
            "nonpar" = "Nonparametric",
            "param" = "Parametric")
        nboot <- x$bootstrap$B
        cat(paste("with ", btype, " Bootstrap standard errors (B = ", nboot, ")\n", sep = ""))
    }
    cat(paste("\nCoefficients for ", statename, " (", x$link$sta, " link):\n", sep = ""))
    printCoefmat(x$sta, digits = digits, signif.legend = FALSE)
    cat(paste("Coefficients for detection (", x$link$det, " link):\n", sep = ""))
    printCoefmat(x$det, digits = digits, signif.legend = FALSE)
    if (x$zeroinfl) {
        cat(paste("Coefficients for zero inflation (", x$link$zif, " link):\n", sep = ""))
        printCoefmat(x$zif, digits = digits, signif.legend = FALSE)
    }
    Stars <- if (x$zeroinfl) {
        !any(is.na(c(array(x$sta), array(x$det), array(x$zif))))
    } else !any(is.na(c(array(x$sta), array(x$det))))
    if (Stars) {
        if (getOption("show.signif.stars") & any(rbind(x$sta, x$det, x$zif)[,4] < 0.1))
            cat("---\nSignif. codes: ", "0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", "\n")
    }
    if (x$distr == "Negative Binomial") {
        SEinfo <- if (is.na(x$var$se)) {
            " (Std. Error: NA)"
        } else {
            paste(" (Std. Error: ", formatC(x$var$se, digits = digits), ")", sep="")
        }
        cat("\nLog Sigma Estimate: ", formatC(x$var$est, digits = digits),
            SEinfo, "\n", sep="")
    }
    cat("\nLog-likelihood:", formatC(x$loglik, digits = digits), 
        "on", x$nobs - x$df.residual, "Df\nAIC =",
        formatC(-2 * x$loglik + 2 * (x$nobs - x$df.residual), digits = digits), "\n")
    if (!x$converged) 
        cat("Warning:\n  Model did not converge\n")
    cat("\n")
    invisible(x)
}


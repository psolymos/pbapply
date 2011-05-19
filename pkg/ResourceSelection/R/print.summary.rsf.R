print.summary.rsf <-
function (x, digits, ...) 
{
    rspf <- if (x$link == "log")
        FALSE else TRUE
    if (missing(digits))
        digits <- max(3, getOption("digits") - 3)
    cat("\nCall:", deparse(x$call,
        width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")
    pp <- if (rspf)
        "Probability " else ""
    ppp <- " model"
    if (!rspf)
        ppp <- " (Exponential RSF) model"
    if (x$link == "logit")
        ppp <- " (Logistic RSPF) model"
    cat(paste("Resource Selection ", pp, "Function", ppp, sep = ""))
    mc <- if (identical(x$m, 0))
        "\nNon-matched Used-Available design\n" else "\nMatched Used-Available design\n"
    cat(paste(mc, "Maximum Likelihood estimates\n", sep = ""))
    if (!is.null(x$bootstrap))
        cat(paste("with Nonparametric Bootstrap standard errors (B = ", x$B, ")\n", sep = ""))

    pp <- if (rspf)
        "probabilities:\n" else "values:\n"
    cat("\nFitted ", pp, sep="")
    print(summary(x$fitted), digits = digits, ...)

    cat(paste("\nCoefficients (", x$link, " link):\n", sep = ""))
#    cfsmat <- if (rspf)
#        x$coefficients else x$coefficients[-1,]
    printCoefmat(x$coefficients, digits = digits, signif.legend = FALSE)

    if (!any(is.na(array(x$coefficients)))) {
        if (getOption("show.signif.stars") & any(x$coefficients[,4] < 0.1))
            cat("---\nSignif. codes: ", "0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", "\n")
    }
    cat("\nLog-likelihood:", formatC(x$loglik, digits = digits), 
#        "on", x$df, "Df\nAIC =",
#        formatC(-2 * x$loglik + 2 * x$df, digits = digits), "\n")
        "\nBIC =",
        formatC(x$bic, digits = digits), "\n")
    if (!is.null(x$hoslem.test)) {
        cat("\nHosmer and Lemeshow goodness of fit (GOF) test:\n")
        cat("X-squared = ", formatC(x$hoslem.test$statistic, digits = digits), 
            ", df = ", x$hoslem.test$parameter,
            ", p-value ", format.pval(x$hoslem.test$p.value, digits = digits), "\n", sep="")
    }
    if (!x$converged) 
        cat("Warning:\n  Model did not converge\n")
    cat("\n")
    invisible(x)
}


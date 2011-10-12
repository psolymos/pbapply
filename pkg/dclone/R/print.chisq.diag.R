print.chisq.diag <-
function (x, digits = max(3, getOption("digits") - 3), ...)
{
    qq <- rbind("Empirical" = summary(x$quantiles$empirical),
        "Theoretical" = summary(x$quantiles$theoretical))
    cat("Mean squared error =", round(x$statistics$ms.error, digits = digits))
    cat("\nr-squared =", round(x$statistics$r.squared, digits = digits))
    if (!is.null(nclones(x)))
        cat("\nNumber of clones =", nclones(x))
    cat("\n\nQuantiles:\n")
    print.default(qq, digits = digits, quote = FALSE)
    probs <- attr(x, "probs")
    invisible(x)
}

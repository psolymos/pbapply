print.chisq.diag <-
function (x, digits = max(3, getOption("digits") - 3), ...)
{
    qq <- rbind("Empirical" = summary(x$quantiles$empirical),
        "Theoretical" = summary(x$quantiles$theoretical))
    cat("Mean squared error:\n", round(x$statistics$ms.error, digits = digits))
    cat("\nr-squared:\n", round(x$statistics$r.squared, digits = digits), "\n")
    cat("\nQuantiles:\n")
    print.default(qq, digits = digits, quote = FALSE)
    invisible(x)
}


plot.chisq.diag <- function(x, 
    xlab = "Theoretical Quantiles",
    ylab = "Empirical Quantiles", line = TRUE, ...)
{
    qq <- x$quantiles
    qqplot(qq$theoretical, qq$empirical, 
        plot.it = TRUE, xlab = xlab, ylab = ylab, ...)
    if (line)
        abline(0, 1, lty = 2)
    invisible(x)
}

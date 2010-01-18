plot.chisq.diag <- function(x, 
    xlab = "Theoretical Quantiles",
    ylab = "Empirical Quantiles", ...)
{
    qq <- x$quantiles
    qqplot(qq$theoretical, qq$empirical, 
        plot.it = TRUE, xlab = xlab, ylab = ylab, ...)
    invisible(x)
}

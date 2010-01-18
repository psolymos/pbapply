plot.chisq.diag <- function(x, 
    xlab = "Theoretical Quantiles",
    ylab = "Empirical Quantiles", qqline = TRUE, ...)
{
    qq <- x$quantiles
    qqplot(qq$theoretical, qq$empirical, 
        plot.it = TRUE, xlab = xlab, ylab = ylab, ...)
    if (qqline) {
        y <- quantile(qq$empirical, c(0.25, 0.75))
        x <- quantile(qq$theoretical, c(0.25, 0.75))
        slope <- diff(y)/diff(x)
        int <- y[1L] - slope * x[1L]
        abline(int, slope, lty = 2)
    }
    invisible(x)
}


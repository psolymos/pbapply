plot.chisq.diag <- 
function(x, main, sub, xlab, ylab, qqline = TRUE, ...)
{
    if (missing(main))
        main <- if (is.null(nclones(x)))
            "" else paste("Number of Clones =", nclones(x))
    if (missing(sub))
        sub <- paste("MS Error =", round(x$statistics$ms.error, 3),
            "\tr-squared =", round(x$statistics$r.squared, 3))
    if (missing(xlab))
        xlab <- "Theoretical Quantiles"
    if (missing(ylab))
        ylab <- "Empirical Quantiles"
    qq <- x$quantiles
    qqplot(qq$theoretical, qq$empirical, plot.it=TRUE, 
        xlab=xlab, ylab=ylab, main=main, sub=sub, ...)
    if (qqline) {
        y <- quantile(qq$empirical, c(0.25, 0.75))
        x <- quantile(qq$theoretical, c(0.25, 0.75))
        slope <- diff(y)/diff(x)
        int <- y[1L] - slope * x[1L]
        abline(int, slope, lty = 2)
    }
    invisible(x)
}


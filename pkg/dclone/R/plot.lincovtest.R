plot.lincovtest <-
function(x, type = "b", ylab = "Estimated coefficients", xlab = "Quantiles", ...)
{
    plot(as.numeric(x), type = type, axes = FALSE, ylab = ylab, xlab = xlab, ...)
    axis(2)
    axis(1, at = 1:length(x), labels = names(x))
    abline(x[1], 0, lty = 2)
    invisible(x)
}

rocplot.svocc <-
function(x, scales=seq(0, 1, 0.01), ylab, xlab, show=TRUE, add=FALSE, ...)
{
    if (missing(ylab))
        ylab <- "Sensitivity (true positive rate)"
    if (missing(xlab))
        xlab <- "1 - Specificity (false positive rate)"
    m <- data.frame(lapply(scales, function(z) (x$occurrence.probabilities * x$detection.probabilities) > z))
    out <- cbind("TRUE"=apply(m, 2, function(z) sum(z & x$y == 0) / (x$nobs - sum(x$y))),
        "FALSE"=apply(m, 2, function(z) sum(z & x$y == 1) / sum(x$y)))
    rownames(out) <- scales
    if (show) {
        if (add) {
            lines(out, ...)
        } else {
            plot(out, type="l", ylab=ylab, xlab=xlab, ...)
        }
    }
    invisible(out)
}


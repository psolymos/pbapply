ladderplot <- function(x, col, ylab, xlab, pch=19, horizontal=TRUE, ...) {
    if (!horizontal)
        .NotYetUsed(horizontal)
    n <- nrow(x)
    d <- 0.5
    space=0.3
    if (ncol(x) != 2)
        stop("'x' must have 2 columns")
    if (space > 1 || space < 0)
        stop("'space' must be > 0 and < 1")
    if (missing(col))
        col <- rep(1, nrow(x))
    if (length(col) < n)
        col <- rep(col, n)[1:n]
    if (missing(ylab))
        ylab <- ""
    if (missing(xlab))
        xlab <- deparse(substitute(x))
    plot(array(x), rep(0, 2*n), type="n", ylim=c(-d/space,d/space), axes=FALSE, 
        ylab=ylab, xlab=xlab, ...)
    axis(1)
    points(x[,1], rep(-d, n), col=col, pch=pch)
    points(x[,2], rep(d, n), col=col, pch=pch)
    for (i in 1:n) {
        lines(x[i, 1:2], c(-d, d), col=col[i])
    }
    lab <- if (is.null(colnames(x)))
        paste(xlab, 1:2, sep=".") else colnames(x)
    text(rep(min(x)+diff(range(x))/2, 2), c(-d*1.5, d*1.5), lab)
    invisible(NULL)
}

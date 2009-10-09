ladderplot.default <-
function(x, col=1, pch=19, lty=1, xlim=c(0.5, ncol(x)+0.5), ylim=range(x), vertical = TRUE, ...)
{
    x <- as.data.frame(x)
    if (NCOL(x) < 2)
        stop("'x' must have at least 2 columns")
    if (length(col) < nrow(x))
        col <- rep(col, nrow(x))[1:nrow(x)]
    if (length(pch) < nrow(x))
        pch <- rep(pch, nrow(x))[1:nrow(x)]
    if (length(lty) < nrow(x))
        lty <- rep(lty, nrow(x))[1:nrow(x)]
    y <- stack(x)
    id <- match(colnames(x),levels(y$ind))
    if (vertical) {
        with(y, stripchart(values ~ ind, pch=pch, ylim=ylim, xlim=xlim, vertical=vertical, col="white", ...))
        lapply(1:ncol(x), function(i) points(cbind(rep(i,nrow(x)), x[,id[i]]), col=col, pch=pch))
        lapply(1:nrow(x), function(i) lines(cbind(id, as.matrix(x)[i,]), col=col[i], lty=lty[i]))
    } else {
        tmp <- xlim
        xlim <- ylim
        ylim <- tmp
        with(y, stripchart(values ~ ind, pch=pch, ylim=ylim, xlim=xlim, vertical=vertical, col="white", ...))
        lapply(1:ncol(x), function(i) points(cbind(x[,id[i]], rep(i,nrow(x))), col=col, pch=pch))
        lapply(1:nrow(x), function(i) lines(cbind(as.matrix(x)[i,], id), col=col[i], lty=lty[i]))
    }
    invisible(NULL)
}


ladderplot <-
function(x, col, pch=19, lty=1, vertical = TRUE, ...)
{
    if (NCOL(x) < 2)
        stop("'x' must have at least 2 columns")
    if (missing(col))
        col <- rep(1, nrow(x))
    if (length(col) < nrow(x))
        col <- rep(col, nrow(x))[1:nrow(x)]
    y <- stack(x)
    if (vertical) {
        xlim <- c(0,ncol(x)+1)
        ylim <- range(x)
        with(y, stripchart(values ~ ind, pch=pch, ylim=ylim, xlim=xlim, vertical=vertical, ...))
        lapply(1:ncol(x), function(i) points(cbind(rep(i,nrow(x)), x[,i]), col=col, pch=pch))
        lapply(1:nrow(x), function(i) lines(cbind(1:ncol(x), as.matrix(x)[i,]), col=col[i], lty=lty))
    } else {
        xlim <- range(x)
        ylim <- c(0,ncol(x)+1)
        with(y, stripchart(values ~ ind, pch=pch, ylim=ylim, xlim=xlim, vertical=vertical, ...))
        lapply(1:ncol(x), function(i) points(cbind(x[,i], rep(i,nrow(x))), col=col, pch=pch))
        lapply(1:nrow(x), function(i) lines(cbind(as.matrix(x)[i,], 1:ncol(x)), col=col[i], lty=lty))
    }
    invisible(NULL)
}


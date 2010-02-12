plot.dcdiag <-
function(x, which = c("all", "lambda.max", "ms.error", "r.squared"), position = "topright", ...)
{
    plotit <- function(param, show.legend, lin, ...) {
        y <- x[[param]]
        pch <- rep(21, length(y))
        pch[y < crit] <- 19

        ylim <- range(0, 1, y/y[1])
        ylab <- paste("Scaled ", param, sep="")
        plot(xval, y/y[1], ylim=ylim, pch=pch, type = "b", lty=1,
            xlab = "Number of clones", ylab=ylab,
            main = param, axes = FALSE, ...)

        axis(1, xval, k)
        axis(2)
        box()
        if (show.legend) {
            legend(position, pch=c(21, 19),
                legend=c(paste(" >= ", round(crit, 2), sep=""),
                paste(" < ", round(crit, 2), sep="")))
        }
        if (lin)
            lines(xval, kmin/k, lty=2)
    }
    which <- match.arg(which)
    k <- x$n.clones
    kmin <- min(k)
    xval <- 1:length(k)
    crit <- getOption("dclone.diag")
    if (which == "all") {
        show.legend <- c(FALSE, FALSE, TRUE)
        lin <- c(TRUE, FALSE, FALSE)
        params <- c("lambda.max", "ms.error", "r.squared")
        opar <- par(mfrow=c(1, 3))
        for (i in 1:3)
            plotit(params[i], show.legend=show.legend[i], lin=lin[i], ...)
        par(opar)
    } else {
        lin <- which == "lambda.max"
        plotit(which, show.legend=TRUE, lin, ...)
    }
    invisible(NULL)
}

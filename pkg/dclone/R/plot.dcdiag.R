plot.dcdiag <-
function(x, which = c("all", "lambda.max", "ms.error", "r.squared", "log.lambda.max"),
position = "topright", ...)
{
    plotit <- function(param, show.legend, lin, log.var, ...) {
        y <- x[[param]]
        pch <- rep(21, length(y))
        pch[y < crit] <- 19
        FUN <- if (log.var) {
            function(x) log(x)
        } else {
            function(x) return(x)
        }
        if (log.var) {
            yy <- log(y/y[1])
            ylab <- paste("log(Scaled ", param, ")", sep="")
        } else {
            yy <- y/y[1]
            ylab <- paste("Scaled ", param, sep="")
        }
        xlab <- Number of clones"
        ylim <- range(0, yy)
        plot(xval, yy, ylim=ylim, pch=pch, type = "b", lty=1,
            xlab = xlab, ylab=ylab,
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
            lines(xval, FUN(kmin/k), lty=2)
    }
    which <- match.arg(which)
    k <- x$n.clones
    kmin <- min(k)
    xval <- 1:length(k)
    log.var <- FALSE
    if (which == "log.lambda.max") {
#        k <- log(k)
#        kmin <- log(kmin)
        xval <- log(k)
        log.var <- TRUE
    }
    crit <- getOption("dclone.diag")
    if (which == "all") {
        show.legend <- c(FALSE, FALSE, TRUE)
        lin <- c(TRUE, FALSE, FALSE)
        params <- c("lambda.max", "ms.error", "r.squared")
        opar <- par(mfrow=c(1, 3))
        for (i in 1:3)
            plotit(params[i], show.legend=show.legend[i], lin=lin[i], log.var=log.var, ...)
        par(opar)
    } else {
        if (which == "log.lambda.max")
            which <- "lambda.max"
        lin <- which == "lambda.max"
        plotit(which, show.legend=TRUE, lin, log.var=log.var, ...)
    }
    invisible(NULL)
}

plot.dctable <-
function(x, which = 1:length(x), position = "topright", box.cex = 0.75, ...)
{
    plot1 <- function(param, show.legend, ...) {
        y <- x[[param]]
        xlim <- range(xval - w/2, xval + w/2)
        ylim <- range(y[,"2.5%"], y[,"97.5%"], y$mean - y$sd, y$mean + y$sd)
        pch <- rep(21, nrow(y))
        if (!is.null(y$r.hat)) {
            pch[y$r.hat < crit] <- 19
        }
        plot(xval, y$mean, ylim=ylim, xlim=xlim, pch=pch, type = "b", lty=2,
            xlab = "Number of clones", ylab="Estimate",
            main = param, axes = FALSE, ...)
        axis(1, xval, k)
        axis(2)
        box()
        if (!is.null(y$r.hat) && show.legend) {
            legend(position, pch=c(21, 19),
                legend=c(paste("R.hat >= ", round(crit, 1), sep=""),
                paste("R.hat < ", round(crit, 1), sep="")))
        }
        errlines(xval, cbind(y$mean - y$sd, y$mean + y$sd))
        errlines(xval, cbind(y[,"25%"], y[,"50%"]), width=w, code=3, type="b")
        errlines(xval, cbind(y[,"50%"], y[,"75%"]), width=w, code=3, type="b")
        points(xval, y[,"2.5%"], pch="x")
        points(xval, y[,"97.5%"], pch="x")
    }
    k <- x[[1]]$n.clones
    xval <- 1:length(k)
    crit <- getOption("dclone.crit")["rhat"]
    nam <- names(x)[which]
    w <- box.cex
    m <- length(which)
    if (m <= 3) {
        nr <- 1
        nc <- m
    } else {
        nr <- min(4, ceiling(m/2))
        nc <- 2
    }
    opar <- par(mfrow=c(nr, nc))
    for (i in 1:m) {
        plot1(nam[i], nam[i] == nam[m], ...)
    }
    par(opar)
    invisible(NULL)
}

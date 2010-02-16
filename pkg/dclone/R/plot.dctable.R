plot.dctable <-
function(x, which = 1:length(x), type=c("all", "var", "logvar"), position = "topright", 
box.cex = 0.75, box.bg = NA, ...)
{
    plotit <- function(param, show.legend, ...) {
        y <- x[[param]]
        xlim <- range(xval - w/2, xval + w/2)
        pch <- rep(21, nrow(y))
        if (!is.null(y$r.hat)) {
            pch[y$r.hat < crit] <- 19
        }
        if (type=="all") {
            ylim <- range(y[,"2.5%"], y[,"97.5%"], y$mean - y$sd, y$mean + y$sd)
            plot(xval, y$mean, ylim=ylim, xlim=xlim, pch=pch, type = "n", lty=2,
                xlab = "Number of clones", ylab="Estimate",
                main = param, axes = FALSE, ...)
            points(xval, y$mean, pch=pch, type = "b", lty=2)
        } else {
            FUN <- switch(type,
                "var" = function(x) return(x),
                "logvar" = function(x) log(x))
            ylim <- switch(type,
                "var" = range(0, 1, y$sd^2/y$sd[1]^2),
                "logvar" = range(0, log(y$sd^2/y$sd[1]^2)))
            ylab <- switch(type,
                "var" = "Scaled Variance",
                "logvar" = "log(Scaled Variance)")
            plot(xval, FUN(y$sd^2/y$sd[1]^2), ylim=ylim, xlim=xlim, pch=pch, type = "b", lty=1,
                xlab = "Number of clones", ylab=ylab,
                main = param, axes = FALSE, ...)
        }
        axis(1, xval, k)
        axis(2)
        box()
        if (!is.null(y$r.hat) && show.legend) {
            legend(position, pch=c(21, 19),
                legend=c(paste("R.hat >= ", round(crit, 1), sep=""),
                paste("R.hat < ", round(crit, 1), sep="")))
        }
        if (type=="all") {
            if (!is.na(box.bg))
                errlines(xval, cbind(y[,"25%"], y[,"75%"]), width=w, code=3, type="b", bg=box.bg)
            errlines(xval, cbind(y$mean - y$sd, y$mean + y$sd))
            errlines(xval, cbind(y[,"25%"], y[,"50%"]), width=w, code=3, type="b")
            errlines(xval, cbind(y[,"50%"], y[,"75%"]), width=w, code=3, type="b")
            points(xval, y[,"2.5%"], pch="x")
            points(xval, y[,"97.5%"], pch="x")
        } else {
            lines(xval, FUN(kmin/k), lty=2)
        }
    }
    if (max(which) > length(x))
        stop("'which' too large")
    type <- match.arg(type)
    k <- x[[1]]$n.clones
    kmin <- min(k)
    xval <- 1:length(k)
    crit <- getOption("dclone.rhat")
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
        plotit(nam[i], nam[i] == nam[m], ...)
    }
    par(opar)
    invisible(NULL)
}

plot.dctable <-
function(x, type = "convergence", position="topleft", box.cex = 1, ...)
{
    if (type == "convergence") {
        y <- x$convergence
        crit <- getOption("dclone.crit")
        plot(y$n.clones, y$lambda.max, type = "b", lty=1, pch=21,
            xlab = "Number of clones", ylab="Convergence statistics",
            main = "DC convergence",
            ylim=c(0, max(array(unlist(y[,-1])), crit)), ...)
        points(y$n.clones, y$p.shapiro, pch=22, type = "b", lty=2)
        abline(crit[1], 0, lty=1)
        abline(crit[2], 0, lty=2)
        legend(position, pch=c(21, 22), lty=c(1,2),
            legend=c("lambda.max", "p.shapiro"))
    } else {
        nam <- names(x$statistics)[type]
        y <- x$statistics[[type]]
        w <- if (length(y$n.clones) > 1)
            min(diff(y$n.clones)) * 0.25 * box.cex else box.cex
        xlim <- range(y$n.clones - w/2, y$n.clones + w/2)
        ylim <- range(y[,"2.5%"], y[,"97.5%"], y$mean - y$sd, y$mean + y$sd)
        pch <- rep(21, nrow(y))
        if (!is.null(y$r.hat)) {
            crit <- getOption("dclone.crit")[3]
            pch[y$r.hat < crit] <- 19
        }
        plot(y$n.clones, y$mean, ylim=ylim, xlim=xlim, pch=pch, type = "b", lty=2,
            xlab = "Number of clones", ylab="Estimate",
            main = nam, ...)
        if (!is.null(y$r.hat) && any(y$r.hat < crit)) {
            legend(position, pch=c(21, 19),
                legend=c(paste("R.hat >= ", round(crit, 1), sep=""),
                paste("R.hat < ", round(crit, 1), sep="")))
        }
        errlines(y$n.clones, cbind(y$mean - y$sd, y$mean + y$sd))
        errlines(y$n.clones, cbind(y[,"25%"], y[,"50%"]), width=w, code=3, type="b")
        errlines(y$n.clones, cbind(y[,"50%"], y[,"75%"]), width=w, code=3, type="b")
        points(y$n.clones, y[,"2.5%"], pch="x")
        points(y$n.clones, y[,"97.5%"], pch="x")
    }
    invisible(NULL)
}

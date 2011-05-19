plot.rsf <-
function(x, type=c("all", "used", "avail"),
ask = prod(par("mfcol")) < x$np && dev.interactive(), ...)
{
    mf <- model.frame(x)
    ff <- x$formula
    y <- x$y
    mf[,as.character(ff[[2]])] <- fitted(x)
    type <- match.arg(type)
    if (type == "all") {
        mf <- mf[order(y),]
        y <- y[order(y)]
    }
    if (type == "used") {
        mf <- mf[y==1,]
        y <- y[y==1]
    }
    if (type == "avail") {
        mf <- mf[y==0,]
        y <- y[y==0]
    }
    if (ask) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
    }
    ylab <- if (class(x)[1] == "rspf")
        "Fitted probabilities" else "Fitted values"
    plot(x$formula, mf, ylab=ylab, ...)
    invisible(x)
}


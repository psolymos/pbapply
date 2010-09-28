dclone.dcdim <-
function(x, n.clones=1, attrib=TRUE, ...)
{
    if (n.clones==1)
        return(x)
    DIM <- dim(x)
    clch <- paste("clone", 1:n.clones, sep=".")
    perm <- attr(x, "perm")
    y <- x
    if (is.null(DIM)) {
        DIM <- length(x)
        DIMNAM <- list(names(x), clch)
    } else {
        ## permuting subscripts
        if (!is.null(perm)) {
            i <- (1:length(DIM))
            i[c(length(DIM), perm)] <- i[c(perm, length(DIM))]
            y <- aperm(x, i)
            DIM <- DIM[i]
        }
        ## if last dim is 1 and 'drop' attr is TRUE, drop it
        DIMNAM <- dimnames(y)
        if (attr(x, "drop") && (DIM[length(DIM)] == 1)) {
            DIM <- DIM[-length(DIM)]
            DNI <- length(DIMNAM)
        } else {
            DNI <- length(DIMNAM) + 1
        }
        if (is.null(DIMNAM))
            DIMNAM <- lapply(1:length(DIM), function(i) NULL)
        DIMNAM[[DNI]] <- clch
    }
    rval <- array(rep(y, n.clones), dim=c(DIM, n.clones), dimnames=DIMNAM)
    ## permuting back subscripts
    if (!is.null(perm)) {
        d <- length(dim(rval))
        i <- 1:d
        i[c(perm, d)] <- i[c(d, perm)]
        rval <- aperm(rval, i)
    }
    if (attrib) {
        attr(rval, "n.clones") <- n.clones
        attr(attr(rval, "n.clones"), "method") <- "dim"
        attr(attr(attr(rval, "n.clones"), "method"), "drop") <- attr(x, "drop")
        attr(attr(attr(rval, "n.clones"), "method"), "perm") <- attr(x, "perm")
    }
    rval
}

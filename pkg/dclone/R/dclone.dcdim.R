dclone.dcdim <-
function(x, n.clones=1, attrib=TRUE, ...)
{
    if (n.clones==1)
        return(x)
    DIM <- dim(x)
    clch <- paste("clone", 1:n.clones, sep=".")
    if (is.null(DIM)) {
        DIM <- length(x)
        DIMNAM <- list(names(x), clch)
    } else {
        ## if last dim is 1 and 'drop' attr is TRUE, drop it
        if (attr(x, "drop") && (DIM[length(DIM)] == 1))
            DIM <- DIM[-length(DIM)]
        DIMNAM <- dimnames(x)
        if (is.null(DIMNAM))
            DIMNAM <- lapply(1:length(DIM), function(i) NULL)
        DIMNAM[[(length(DIMNAM) + 1)]] <- clch
    }
    rval <- array(rep(x, n.clones), dim=c(DIM, n.clones), dimnames=DIMNAM)
    if (attrib) {
        attr(rval, "n.clones") <- n.clones
        attr(attr(rval, "n.clones"), "method") <- "dim"
    }
    rval
}

as.mcmc.list.dc.default <-
function(x, n.clones=nclones(x), force=FALSE, ...)
{
    if ((!is.null(n.clones) && n.clones > 1) || force) {
        attr(x, "n.clones") <- n.clones
        class(x) <- c("mcmc.list.dc", class(x))
    }
    x
}


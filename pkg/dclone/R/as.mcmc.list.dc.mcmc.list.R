as.mcmc.list.dc.mcmc.list <-
function(x, n.clones=nclones(x), ...)
{
    class(x) <- c("mcmc.list.dc", class(x))
    attr(x, "n.clones") <- n.clones
    x
}


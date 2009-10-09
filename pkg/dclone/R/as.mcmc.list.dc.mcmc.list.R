as.mcmc.list.dc.mcmc.list <-
function(x, ...)
{
    class(x) <- c("mcmc.list.dc", class(x))
    attr(x, "n.clones") <- nclones(x)
    x
}


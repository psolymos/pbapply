"[.mcmc.list.dc" <- 
function (x, i, j, drop = TRUE) 
{
    class(x) <- "mcmc.list"
    #out <- coda:::"[.mcmc.list"(x=x, i=i, j=j, drop=drop)
    out <- as(x, "mcmc.list")[i=i, j=j, drop=drop]
    attr(out, "n.clones") <- attr(x, "n.clones")
    class(out) <- c("mcmc.list.dc", "mcmc.list")
    out
}

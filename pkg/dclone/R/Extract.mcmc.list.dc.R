"[.mcmc.list.dc" <- 
function (x, i, j, drop = TRUE) 
{
    out <- x[i=i, j=j, drop=drop]
    attr(out, "nclones") <- attr(x, "nclones")
    out
}

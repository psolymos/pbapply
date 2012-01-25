stack.mcmc.list <- 
function(x, ...) 
{
    ar <- as.array(x)
    vn <- varnames(x)
    if (is.null(vn))
        vn <- paste("variable", 1:nvar(x), sep="_")
    data.frame(iter=rep(time(x), nvar(x)*nchain(x)),
        variable=rep(rep(vn, each=niter(x)), nchain(x)),
        chain=as.factor(rep(1:nchain(x), each=niter(x)*nvar(x))),
        value=c(ar))
}

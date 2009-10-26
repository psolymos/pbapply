as.mcmc.list.bugs <-
function(x, ...)
{
    sarr <- x$sims.array
    sarr <- sarr[,,dimnames(sarr)[[3]] != "deviance"]
    res <- lapply(1:x$n.chains, function(i) sarr[,i,])
    ## thin != values can cause problems
    niter <- nrow(res[[1]])
    start <- x$n.burnin+1
    end <- x$n.iter
    thin <- x$n.thin
    nobs <- floor((end - start)/thin + 1)
    if (niter < nobs) 
        stop("can't coerce 'bugs' object as 'mcmc.list',\nconsider refitting the model with 'n.thin = 1'")
    res <- lapply(res, function(z) mcmc(data = z,
        start = start, end = end, thin = thin))
    res <- as.mcmc.list(res)
    n.clones <- attr(x, "n.clones")
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}

mclapplySB <- 
function(X, FUN, ..., mc.set.seed = TRUE,
         mc.silent = FALSE, mc.cores = 1L,
         mc.cleanup = TRUE, mc.allow.recursive = TRUE, size = 1)
{
    fun <- match.fun(fun)
    s <- clusterSplitSB(1:mc.cores, X, size)
    id <- clusterSplitSB(cl, 1:length(X), size)

#    res <- clusterApply(cl, s, lapply, fun, ...)
    res <- mclapply(X, 
        FUN, ..., 
        mc.preschedule = TRUE, 
        mc.set.seed = TRUE,
        mc.silent = FALSE, 
        mc.cores = 1L,
        mc.cleanup = TRUE, 
        mc.allow.recursive = TRUE)

    res <- docall(c, res)
    res <- res[order(unlist(id))]
    res
}

x <- list(1, 2, 3, 4)
size <- 1:4
mc <- 2
X <- clusterSplitSB(1:mc, x, size)

mclapply(X, function(z) z^2, mc.cores=mc)
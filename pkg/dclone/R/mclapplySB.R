mclapplySB <- 
function(X, FUN, ..., 
mc.preschedule = TRUE, mc.set.seed = TRUE,
mc.silent = FALSE, mc.cores = 1L,
mc.cleanup = TRUE, mc.allow.recursive = TRUE, 
size = 1)
{
    if (length(unique(size) == 1)) {
        res <- mclapply(X, 
            FUN, ..., 
            mc.preschedule = mc.preschedule, 
            mc.set.seed = mc.set.seed,
            mc.silent = mc.silent, 
            mc.cores = mc.cores,
            mc.cleanup = mc.cleanup, 
            mc.allow.recursive = mc.allow.recursive)
    } else {
        s <- clusterSplitSB(1:mc.cores, X, size)
        id <- clusterSplitSB(1:mc.cores, 1:length(X), size)
        mcfun <- function(x, ...)
            lapply(x, FUN, ...)
        res <- mclapply(s, 
            mcfun, ..., 
            mc.preschedule = mc.preschedule, 
            mc.set.seed = mc.set.seed,
            mc.silent = mc.silent, 
            mc.cores = mc.cores,
            mc.cleanup = mc.cleanup, 
            mc.allow.recursive = mc.allow.recursive)
        res <- do.call(c, res)
        res <- res[order(unlist(id))]
    }
    res
}



parLapplySB <- function(cl, x, fun, size=1, load.balancing = FALSE, ...) {
    fun <- match.fun(fun)
    clusterfun <- if (load.balancing)
        match.fun(clusterApplyLB) else match.fun(clusterApply)
    s <- clusterSplitSB(cl, x, size)
    id <- clusterSplitSB(cl, 1:length(x), size)
    res <- clusterfun(cl, s, lapply, fun, ...)
    res <- docall(c, res)
    res <- res[order(unlist(id))]
    res
}


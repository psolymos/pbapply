parLapplySB <- 
function(cl, x, size = 1, fun, ...)
{
    fun <- match.fun(fun)
    s <- clusterSplitSB(cl, x, size)
    id <- clusterSplitSB(cl, 1:length(x), size)
    res <- clusterApply(cl, s, lapply, fun, ...)
    res <- docall(c, res)
    res <- res[order(unlist(id))]
    res
}

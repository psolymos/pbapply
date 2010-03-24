clusterSeed <-
function(cl, seed, kind = "default", normal.kind = "default")
{
    n <- length(cl)
    seed <- rep(seed, n)[1:n]
    kind <- rep(kind, n)[1:n]
    normal.kind <- rep(normal.kind, n)[1:n]
    seedlist <- lapply(1:n, function(i) 
        list(seed=seed[i], kind=kind[i], normal.kind=normal.kind[i]))
    fun <- function(z)
        set.seed(z$seed, z$kind, z$normal.kind)
    clusterApply(cl, seedlist, fun)
    return(seedlist)
}

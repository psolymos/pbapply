cluster.split <- function(cl, seq, size = 1) {
    m <- length(seq)
    size <- rep(size, m)[1:m]
    ## equal size
    require(snow)
    if (length(unique(size)) == 1)
        return(clusterSplit(cl, seq))
    ## unequal size
    n <- length(cl)
    id <- 1:m
    ord <- order(size, decreasing = TRUE)
    size <- size[ord]
    id <- id[ord]
    w <- matrix(0, max(1,m-n+1), n)
    s <- matrix(NA, max(1,m-n+1), n)
    w[1,1:n] <- size[1:n]
    s[1,1:n] <- id[1:n]
    if (n < m)
        for (i in 2:nrow(w)) {
            j <- which(colSums(w) == min(colSums(w)))[1]
            w[i, j] <- size[i+n-1]
            s[i, j] <- id[i+n-1]
        }
    spl <- lapply(1:n, function(i) s[!is.na(s[,i]),i])
    rval <- lapply(spl, function(z) seq[z])
    if (n > length(rval))
        for (i in (length(rval)+1):n) {
            rval[[i]] <- numeric(0)
        }
    rval
}

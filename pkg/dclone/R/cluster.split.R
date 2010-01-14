cluster.split <- function(cl, seq, size=NULL) {
    require(snow)
    if (is.null(size))
        return(clusterSplit(cl, seq))
    n <- length(cl)
    m <- length(seq)
    id <- 1:m
    size <- rep(size, m)[1:m]
    w <- matrix(0, m-n+1, n)
    s <- matrix(NA, m-n+1, n)
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
    rval
}

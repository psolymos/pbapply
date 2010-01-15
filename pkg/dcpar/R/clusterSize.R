clusterSize <- function(n, size) {
    fun <- function(n, seq, size) {
        m <- length(size)
        w <- matrix(0, m-n+1, n)
        s <- matrix(NA, m-n+1, n)
        w[1,1:n] <- size[1:n]
        s[1,1:n] <- seq[1:n]
        if (n < m)
            for (i in 2:nrow(w)) {
                j <- which(colSums(w) == min(colSums(w)))[1]
                w[i, j] <- size[i+n-1]
                s[i, j] <- seq[i+n-1]
            }
        list(seq=s, size=w)
    }
    seq <- 1:length(size)
    ord <- order(size, decreasing = TRUE)
    size <- size[ord]
    seq <- seq[ord]
    n <- min(n, length(size))
    res <- lapply(1:n, fun, seq = seq, size = size)
    csmax <- sapply(res, function(z) max(colSums(z$size)))
    csmin <- sapply(res, function(z) min(colSums(z$size)))
    data.frame(n=1:n,min=csmin,max=csmax)
}

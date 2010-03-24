clusterSize <-
function(size)
{
    m <- length(size)
    if (m==1)
        stop("'length(size)' > 1 is needed")
    res1 <- sapply(2:m, function(i) plotClusterSize(i, size, "none", plot=FALSE))
    res2 <- sapply(2:m, function(i) plotClusterSize(i, size, "load", plot=FALSE))
    res3 <- sapply(2:m, function(i) plotClusterSize(i, size, "size", plot=FALSE))
    r0 <- sum(size)
    res <- data.frame(workers=1:m, none=c(r0, res1),
        load=c(r0, res2), size=c(r0, res3), both=c(r0, res3))
    res
}

clusterSize <-
function(size)
{
    m <- length(size)
    if (m==1)
        stop("'length(size)' > 1 is needed")
    res1 <- sapply(2:m, function(i) plotClusterSize(i, size, "none", plot=FALSE))
    res2 <- sapply(2:m, function(i) plotClusterSize(i, size, "load", plot=FALSE))
    res3 <- sapply(2:m, function(i) plotClusterSize(i, size, "size", plot=FALSE))
    data.frame(workers=2:m,none=res1,load=res2,size=res3,both=res3)
}

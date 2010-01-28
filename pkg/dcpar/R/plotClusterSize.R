plotClusterSize <-
function(n, size, balancing=c("none","load","size","both"), plot=TRUE)
{
    clusterSplitLB <- function(cl, seq, size = 1) {
        tmp1 <- tmp2 <- matrix(NA, length(cl), length(seq))
        tmp1[1,1] <- size[1]
        tmp2[1,1] <- 1
        for (i in 2:length(seq)) {
            rs <- rowSums(tmp1, na.rm=TRUE)
            tmp1[which(rs == min(rs))[1],i] <- size[i]
            tmp2[which(rs == min(rs))[1],i] <- i
        }
        lapply(1:n, function(i) tmp2[i,!is.na(tmp2[i,])])
    }
    if (n==1)
        stop("'n' > 1 is needed")
    m <- length(size)
    seq <- 1:m
    balancing <- match.arg(balancing)
    cl <- 1:n
    x <- switch(balancing,
        "none" = clusterSplit(cl, seq),
        "load" = clusterSplitLB(cl, seq, m:1),
        "size" = clusterSplitSB(cl, seq, size),
        "both" = clusterSplitSB(cl, seq, size))
    size <- rep(size, m)[1:m]
    s <- switch(balancing,
        "none" = clusterSplit(cl, size),
        "load" = clusterSplitLB(cl, size, m:1),
        "size" = clusterSplitSB(cl, size, size),
        "both" = clusterSplitSB(cl, size, size))
    x2 <- lapply(s, cumsum)
    x1 <- lapply(1:n, function(i) x2[[i]] - s[[i]])

    offset <- 0.1
    y <- 1:n
    y1 <- y+(0.5-offset)
    y2 <- y-(0.5-offset)

    if (plot) {
        plot.new()
        plot.window(xlim=range(x1,x2),ylim=range(y1,y2))
        axis(side=1)
        axis(side=2, at=y, tick=FALSE)
        main <- switch(balancing,
            "none" = "No Balancing",
            "load" = "Load Balancing",
            "size" = "Size Balancing",
            "both" = "Size and Load Balancing")
        title(main=main, xlab="Approximate Processing Time", ylab="Workers",
            sub=paste("Max =", max(sapply(x2, max))))
        for (i in 1:n) {
            for (j in 1:length(x[[i]])) {
                polygon(c(x1[[i]][j], x2[[i]][j], x2[[i]][j], x1[[i]][j]), c(y1[i], y1[i], y2[i], y2[i]))
                text(mean(c(x1[[i]][j], x2[[i]][j])), y[i], x[[i]][j])
            }
        }
    }
    invisible(max(sapply(x2, max)))
}

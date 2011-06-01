makeUsedAvail.default <- 
function(x, y, ...) 
{
    YNAME <- deparse(substitute(y))
    YNAME <- gsub("$", ".", YNAME, fixed=TRUE)
    YNAME <- gsub("@", ".", YNAME, fixed=TRUE)
    y <- as.integer(y)
    if (!all(unique(y) %in% c(0L,1L)))
        stop("'y' must be in c(0, 1)")
    avail <- data.frame(status=0, x)
    used <- data.frame(status=1, x[y==1,])
    out <- rbind(used, avail)
    colnames(out)[1] <- YNAME
    out
}

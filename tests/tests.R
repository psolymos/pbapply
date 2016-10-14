## --- standard examples ---

library(pbapply)

example(pboptions, run.dontrun = TRUE)
example(pbapply, run.dontrun = TRUE)
example(lapply, run.dontrun = TRUE)
example(apply, run.dontrun = TRUE)
example(splitpb, run.dontrun = TRUE)
example(timerProgressBar, run.dontrun = TRUE)

## --- test for NULL case in lapply ---

l <- list(a = 1, 2, c = -1)
f <- function(z) if (z < 0) return(NULL) else return(2 * z)
r1 <- lapply(l, f)
r2 <- pblapply(l, f)
r1
r2
stopifnot(identical(r1, r2))

## --- timings ---

if (FALSE) {

#library(plyr)
## from http://ryouready.wordpress.com/2010/01/11/progress-bars-in-r-part-ii-a-wrapper-for-apply-functions/#comment-122
lapply_pb <-
function(X, FUN, ...)
{
    env <- environment()
    pb_Total <- length(X)
    counter <- 0
    pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)
    wrapper <- function(...){
        curVal <- get("counter", envir = env)
        assign("counter", curVal +1 ,envir = env)
        setTxtProgressBar(get("pb", envir = env), curVal + 1)
        FUN(...)
    }
    res <- lapply(X, wrapper, ...)
    close(pb)
    res
}

i <- seq_len(100)
t1 <- system.time(lapply(i, function(i) Sys.sleep(0.1)))
t2 <- system.time(lapply_pb(i, function(i) Sys.sleep(0.1)))
#t3 <- system.time(l_ply(i, function(i) Sys.sleep(0.1), .progress="text"))
t4 <- system.time(pblapply(i, function(i) Sys.sleep(0.1)))

#f <- function(n, s=0.1) {
#    i <- seq_len(n)
#    t1 <- system.time(lapply(i, function(i) Sys.sleep(s)))
#    t2 <- system.time(lapply_pb(i, function(i) Sys.sleep(s)))
#    t3 <- system.time(l_ply(i, function(i) Sys.sleep(s), .progress="text"))
#    t4 <- system.time(pblapply(i, function(i) Sys.sleep(s)))
#    rbind(cbind(t1, t2, t3, t4), expected=n*s)
#}

#n <- c(10, 50, 100, 200, 500)
#x <- lapply(n, f)
#m <- t(sapply(x, function(z) z["elapsed",] - z["expected",]))

#op <- par(mfrow=c(1,2))
#matplot(n, m, type="b", lty=1, ylab="Overhead (sec)", xlab="# iterations")
#legend("topleft", bty="n", col=1:4, pch=as.character(1:4), text.col=1:4,
#    legend=c("lapply","lapply_pb","l_ply","pbapply"))
#matplot(n, m/n, type="b", lty=1, ylab="Overhead / # iterations (sec)", xlab="# iterations")
#par(op)

}

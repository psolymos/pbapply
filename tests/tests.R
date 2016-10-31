## --- standard examples ---

library(pbapply)

example(apply)
example(lapply)

## run examples without progress bar
pboptions(type = "none")
example(splitpb, run.dontrun = TRUE)
example(timerProgressBar, run.dontrun = TRUE)
example(pbapply, run.dontrun = TRUE)
example(pboptions, run.dontrun = TRUE)

## run examples with progress bar
pboptions(type = "timer")
example(splitpb, run.dontrun = TRUE)
example(timerProgressBar, run.dontrun = TRUE)
example(pbapply, run.dontrun = TRUE)
example(pboptions, run.dontrun = TRUE)

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

}

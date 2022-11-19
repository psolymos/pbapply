#devtools::install_github("psolymos/pbapply")

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

## check potential changes in formal arguments
check_args <- function(fun1, fun2, cl=TRUE) {
    f1 <- formals(fun1)
    f2 <- formals(fun2)
    args1 <- names(f1)
    args2cl <- names(f2)
    args2 <- if (cl)
        args2cl[seq_len(length(args2cl)-1L)] else args2cl
    vals1 <- unname(f1)
    vals2cl <- unname(f2)
    vals2 <- if (cl)
        vals2cl[seq_len(length(vals2cl)-1L)] else vals2cl
    if (length(args1) != length(args2)) {
        msg <- c("Number of arguments is different:\n - fun1 [",
            length(args1), "]: ", paste0(args1, collapse=", "),
            "\n - fun2 [",
            length(args2), "]: ", paste0(args2, collapse=", "))
        stop(paste0(msg, collapse=""))
    }
    if (!all(args1 == args2)) {
        msg <- c("Argument mismatches:\n  - in fun1 but not fun2: ", 
            paste0(setdiff(args1, args2), collapse=", "),
            "\n  - in fun2 but not fun1: ", 
            paste0(setdiff(args2, args1), collapse=", "))
        stop(paste0(msg, collapse=""))
    }
    if (!all(sapply(1:length(vals1),function(i) identical(vals1[[i]], vals2[[i]])))) {
        msg <- c("Number of arguments is different:\n - fun1: ",
            paste0(vals1, collapse=", "),
            "\n - fun2: ",
            paste0(vals2, collapse=", "))
        stop(paste0(msg, collapse=""))
    }
    invisible(TRUE)
}

check_args(lapply, pblapply)
check_args(lapply, pbwalk)
check_args(apply, pbapply)
check_args(sapply, pbsapply)
check_args(replicate, pbreplicate)
check_args(tapply, pbtapply)
check_args(eapply, pbeapply)
check_args(vapply, pbvapply)
check_args(by, pbby)

check_args(mapply, pbmapply, cl=FALSE)
check_args(Map, pbMap, cl=FALSE)
check_args(.mapply, pb.mapply, cl=FALSE)

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

## --- knitr related tests ---

if (FALSE) {

sink("~/repos/pbapply/tests/pb.Rmd")
cat("---
title: \"Test pbapply with knitr\"
date: \"`r format(Sys.time(), '%B %d, %Y')`\"
output: pdf_document
---

# Introduction

Play nice!

```{r setup}
library(knitr)
library(pbapply)
interactive()
getOption(\"knitr.in.progress\")
is.null(getOption(\"knitr.in.progress\"))
pboptions()$type
```

```{r chunk}
pbsapply(1:100, function(z) {Sys.sleep(0.01); sqrt(z)})
```
")
sink()
#knitr::knit("~/repos/pbapply/tests/pb.Rmd", "~/repos/pbapply/tests/pb.md")
unlink("~/repos/pbapply/tests/pb.Rmd")
unlink("~/repos/pbapply/tests/pb.md")

}

## --- tests for issue #17: single core in cl ---

f <- function(i) Sys.sleep(0.1)

library(parallel)
cl <- makeCluster(1L)

pblapply(1:10, f, cl = cl)

stopCluster(cl)

## --- tests for issue #33: return empty list for empty vector ---

tmp1 <- lapply(character(0), identity)
tmp2 <- pblapply(character(0), identity)
stopifnot(length(tmp1) == length(tmp2))
stopifnot(identical(tmp1, tmp2))

tmp1 <- sapply(character(0), identity)
tmp2 <- pbsapply(character(0), identity)
stopifnot(length(tmp1) == length(tmp2))
stopifnot(identical(tmp1, tmp2))

tmp1 <- apply(matrix(numeric(0), 0, 0), 1, identity)
tmp2 <- pbapply(matrix(numeric(0), 0, 0), 1, identity)
stopifnot(length(tmp1) == length(tmp2))
stopifnot(identical(tmp1, tmp2))

tmp1 <- apply(matrix(numeric(0), 0, 0), 2, identity)
tmp2 <- pbapply(matrix(numeric(0), 0, 0), 2, identity)
stopifnot(length(tmp1) == length(tmp2))
stopifnot(identical(tmp1, tmp2))

## --- tests for issue #48: pbwalk ---

tmp <- tempdir()
# f <- function(i, dir) {
#     x <- rnorm(100)
#     png(file.path(dir, paste0("plot-", i, ".png")))
#     hist(x, col=i)
#     dev.off()
#     x
# }
f <- function(i, dir) {
    x <- data.frame(i=i, j=rnorm(5))
    write.csv(x, row.names=FALSE, file=file.path(dir, paste0("file-", i, ".csv")))
    x
}
# pblapply(1:3, f, dir=tmp)
pbwalk(1:3, f, dir=tmp)
# unlink(file.path(tmp, paste0("plot-", 1:3, ".png")))
unlink(file.path(tmp, paste0("file-", 1:3, ".csv")))

pbwalk(1:3, f, dir=tmp, cl=2)
# unlink(file.path(tmp, paste0("plot-", 1:3, ".png")))
unlink(file.path(tmp, paste0("file-", 1:3, ".csv")))

cl <- parallel::makeCluster(2)
pbwalk(1:3, f, dir=tmp, cl=cl)
parallel::stopCluster(cl)
# unlink(file.path(tmp, paste0("plot-", 1:3, ".png")))
unlink(file.path(tmp, paste0("file-", 1:3, ".csv")))

## this could be a quartz issue ...
# f <- function(i, dir) {
#     x <- rnorm(100)
#     png(file.path(dir, paste0("plot-", i, ".png")))
#     hist(x, col=i)
#     dev.off()
#     x
# }
## all this works
# f(1, tmp)
# pbapply::pblapply(1:3, f, dir=tmp)
# pbapply::pbwalk(1:3, f, dir=tmp)
# unlink(file.path(tmp, paste0("plot-", 1:3, ".png")))
## all this does not
# pbapply::pbwalk(1:3, f, dir=tmp, cl=2)
# parallel::mclapply(1:3, f, dir=tmp, mc.cores=2)


library(future)

l <- list(a = 1, 2, c = -1)
f <- function(z) {
    Sys.sleep(0.1)
    if (z < 0) return(NULL) else return(2 * z)
}

plan(sequential)
r2 <- pblapply(l, f, cl = "future")

plan(multisession, workers = 2)
r2 <- pblapply(l, f, cl = "future")

cl <- parallel::makeCluster(2)
plan(cluster, workers = cl)
r2 <- pblapply(l, f, cl = "future")
parallel::stopCluster(cl)
plan(sequential)

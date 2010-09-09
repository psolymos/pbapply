library(pbapply)
example(pboptions)
example(pbapply)
example(lapply)
example(apply)

##
#library(plyr)
## from http://ryouready.wordpress.com/2010/01/11/progress-bars-in-r-part-ii-a-wrapper-for-apply-functions/#comment-122
lapply_pb <- function(X, FUN, ...)
{
 env <- environment()
 pb_Total <- length(X)
 counter <- 0
 pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)   

 # wrapper around FUN
 wrapper <- function(...){
   curVal <- get("counter", envir = env)
   assign("counter", curVal +1 ,envir=env)
   setTxtProgressBar(get("pb", envir=env), curVal +1)
   FUN(...)
 }
 lapply(X, wrapper, ...)
 close(pb)
}
system.time(x1 <- lapply(1:10, function(i) Sys.sleep(0.2)))
system.time(x1 <- lapply_pb(1:10, function(i) Sys.sleep(0.2)))
#system.time(x1 <- l_ply(1:10, function(i) Sys.sleep(0.2), .progress=create_progress_bar(name = "text")))
system.time(x1 <- pblapply(1:10, function(i) Sys.sleep(0.2)))

#opb <- pboptions(char=c("-\r", "\\\r", "|\r", "/\r"),style=1)
#system.time(x1 <- pblapply(1:10, function(i) Sys.sleep(0.2)))

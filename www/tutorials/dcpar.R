## We learn now to run MCMC chains (even >4) in parallel, how to evaluate
## simulations or bootstrap iteration in parallel, how to do data cloning
## with different number of clones in parallel.

## Same prerequisites as for Lecture 1, plus the snow R package
## installed. I will distribute copies of the dcpar package (very
## developmental).

## Topics to cover:
## - parallel simulations/bootstrap with the snow package
## - parallel MCMC chain computations, issues with random numbers
## - using snow functions inside other functions: difficulties and solutions
## - parallel data cloning with different number of clones: size balancing 

## recommended reading: Simple Parallel Statistical Computing in R
## http://www.bepress.com/uwbiostat/paper193/

## by loading dcpar, we get dclone and snow as well
library(dcpar)

## 1. parallel simulations/bootstrap with the snow package

## snow = Simple Network Of Workstations

## virtual connection between processes
##    * Socket
##    * PVM (Parallel Virtual Machine)
##    * MPI (Message Passing Interface)

## for multi-core machines, we use SOCK

## start a cluster
cl <- makeCluster(2, type = "SOCK")
cl
## end a cluster
stopCluster(cl)

## clusterCall(cl, fun, ...)

## clusterEvalQ(cl, expr)

## clusterApply(cl, seq, fun, ...)
## clusterApplyLB(cl, seq, fun, ...)

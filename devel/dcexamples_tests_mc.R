## testing forking type parallelism
DIR <- if (.Platform$OS.type == "windows")
    "c:/svn/dcr/devel" else "/home/peter/svn/dcr/devel"
setwd(paste(DIR, "/tests", sep=""))
source(paste(DIR, "/global_vars.R", sep=""))
library(dcmle)
load.module("glm")
load.module("dic")
jags_example <- function(topic, renv, tenv, ...) {
    x <- sourceDcExample(topic, assign.global=FALSE)
    t0 <- proc.time()
    out <- try(dcmle:::dcmle(x, ...))
    pt <- proc.time() - t0
    assign(topic, pt, envir=tenv)
    assign(topic, out, envir=renv)
}
#options(dcmle.href="c:/svn/dcr/www/examples")
## this is for k=1
(topic <- listDcExamples()$topic)
timer1 <- new.env(hash=FALSE)
timer2 <- new.env(hash=FALSE)
res1 <- new.env(hash=FALSE)
res2 <- new.env(hash=FALSE)
#options("mc.cores"=3) # - this overwrites NULL
cl <- 3
#cl <- makePSOCKcluster(3)
#clusterEvalQ(cl, library(dcmle))
#parLoadModule(cl, "glm")
#parLoadModule(cl, "dic")
dcop <- dcoptions(verbose=0)
cat("\n\n## <<<<<<<<<<<<<<    ", date(), "    >>>>>>>>>>>>>>>>>\n\n")
cat("\n\n## START <<<<<<<<<<<<<<    paramecium    >>>>>>>>>>>>>>>>>\n")
sourceDcExample("paramecium", assign.global=TRUE)
str(paramecium)
paramecium@model
out1 <- dcmle:::dcmle(paramecium, n.clones=1, 
        n.adapt=n.adapt, n.update=n.update, n.iter=n.iter, n.chains=n.chains, thin=thin)
out2 <- dcmle:::dcmle(paramecium, n.clones=1, cl=3,
        n.adapt=n.adapt, n.update=n.update, n.iter=n.iter, n.chains=n.chains, thin=thin)
cat("\n## END   <<<<<<<<<<<<<<    paramecium    >>>>>>>>>>>>>>>>>\n\n")
for (i in topic) {
    cat("\n\n## START <<<<<<<<<<<<<<    ", i, "    >>>>>>>>>>>>>>>>>\n")
    cat("\n## -- seq --\n")
    jags_example(i, n.clones=1, renv=res1, tenv=timer1, 
        n.adapt=n.adapt, n.update=n.update, n.iter=n.iter, n.chains=n.chains, thin=thin)
    cat("\n## -- par --\n")
    jags_example(i, n.clones=1, cl=cl, renv=res2, tenv=timer2, 
        n.adapt=n.adapt, n.update=n.update, n.iter=n.iter, n.chains=n.chains, thin=thin)
    cat("\n## END   <<<<<<<<<<<<<<    ", i, "    >>>>>>>>>>>>>>>>>\n\n")
}
#stopCluster(cl)
#dcoptions(dcop)
t1 <- matrix(0, length(topic), 3)
colnames(t1) <- names(timer1[[as.character(topic[1])]])[1:3]
rownames(t1) <- topic
t2 <- t1
for (i in 1:length(topic)) {
    t1[i,] <- timer1[[as.character(topic[i])]][1:3]
    t2[i,] <- timer2[[as.character(topic[i])]][1:3]
}
gs1 <- sapply(as.list(res1), nvar)
nc1 <- sapply(as.list(res1), nchain)
nc2 <- sapply(as.list(res2), nchain)
## this for DC
timer3 <- new.env(hash=FALSE)
timer4 <- new.env(hash=FALSE)
timer5 <- new.env(hash=FALSE)
timer6 <- new.env(hash=FALSE)
res3 <- new.env(hash=FALSE)
res4 <- new.env(hash=FALSE)
res5 <- new.env(hash=FALSE)
res6 <- new.env(hash=FALSE)
k <- if (LONG)
    c(1,2,4,6) else 1:2
cl <- 8
#cl <- makePSOCKcluster(8)
#clusterEvalQ(cl, library(dcmle))
#parLoadModule(cl, "glm")
#parLoadModule(cl, "dic")
#dcop <- dcoptions(verbose=0)
topic <- c("paramecium",                                          # misc
    "blocker","dyes","epil","equiv","pump","salm","seeds","rats", # vol 1
    "beetles","birats","dugongs","eyes","hearts","jaw","orange")  # vol 2
for (i in topic) {
    cat("\n\n## START <<<<<<<<<<<<<<    ", paste(i, "_DC", sep=""), "    >>>>>>>>>>>>>>>>>\n")
    cat("\n## -- seq --\n")
    jags_example(i, n.clones=k, renv=res3, tenv=timer3, 
        n.adapt=n.adapt, n.update=n.update, n.iter=n.iter, n.chains=n.chains, thin=thin)
    cat("\n## -- bal --\n")
    jags_example(i, n.clones=k, cl=cl, partype="balancing", renv=res4, tenv=timer4, 
        n.adapt=n.adapt, n.update=n.update, n.iter=n.iter, n.chains=n.chains, thin=thin)
    cat("\n## -- par --\n")
    jags_example(i, n.clones=k, cl=cl, partype="parchains", renv=res5, tenv=timer5, 
        n.adapt=n.adapt, n.update=n.update, n.iter=n.iter, n.chains=n.chains, thin=thin)
    cat("\n## -- both --\n")
    jags_example(i, n.clones=k, cl=cl, partype="both", renv=res6, tenv=timer6, 
        n.adapt=n.adapt, n.update=n.update, n.iter=n.iter, n.chains=n.chains, thin=thin)
    cat("\n## END   <<<<<<<<<<<<<<    ", paste(i, "_DC", sep=""), "    >>>>>>>>>>>>>>>>>\n\n")
}
#stopCluster(cl)
dcoptions(dcop)
t3 <- matrix(0, length(topic), 3)
colnames(t3) <- names(timer3[[as.character(topic[1])]])[1:3]
rownames(t3) <- topic
t6 <- t5 <- t4 <- t3
for (i in 1:length(topic)) {
    t3[i,] <- timer3[[as.character(topic[i])]][1:3]
    t4[i,] <- timer4[[as.character(topic[i])]][1:3]
    t5[i,] <- timer5[[as.character(topic[i])]][1:3]
    t6[i,] <- timer6[[as.character(topic[i])]][1:3]
}
gs3 <- sapply(as.list(res3), nvar)
nc3 <- sapply(as.list(res3), nchain)
nc4 <- sapply(as.list(res4), nchain)
nc5 <- sapply(as.list(res5), nchain)
nc6 <- sapply(as.list(res6), nchain)

(z1 <- cbind(round(cbind(seq=t1[,3], pch=t2[,3]) / t1[,3], 3), graph_size=gs1))
(z2 <- cbind(round(cbind(seq=t3[,3], bal=t4[,3], pch=t5[,3], both=t6[,3]) / t3[,3], 3), graph_size=gs3))

n.chains
lapply(list(nc1, nc2, nc3, nc4, nc5, nc6), range)

x <- readLines("dcexamples_tests_mc.log")
err <- c(grep("rror", x), grep("arning", x))
fal <- grep("d error", x)
err <- err[!(err %in% fal)]
if (length(err)) {
    beg <- c(grep("## START <<<<<<<<<<<<<<", x), length(x))[-1]
    fin <- grep("## END   <<<<<<<<<<<<<<", x)
    top <- gsub("     >>>>>>>>>>>>>>>>>", "", gsub("## START <<<<<<<<<<<<<<     ", "", x[beg]))
    y <- character(length(x))
    y[1:(beg[1]-1)] <- "begin"
    for (i in 1:(length(beg)-1))
        y[beg[i]:(beg[i+1]-1)] <- top[i]
    y[(fin[length(fin)]:length(y))] <- "endmatter"
    y <- y[err]
    cat("\n\n##       <<<<<<<<<<<<<<    Errors/Warnings found    >>>>>>>>>>>>>>>>>\n\n")
    data.frame(Line=err, Topic=y, Text=x[err])
} else cat("\n\n##       <<<<<<<<<<<<<<    OK -- No Errors/Warnings found    >>>>>>>>>>>>>>>>>\n\n")
if (SAVE)
    save(list=ls(), file="dcexamples_tests_mc.Rdata")
rm(list = ls())
## EOF


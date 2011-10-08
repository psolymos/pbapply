setwd("c:/svn/dcr/devel/tests")
library(dcmle)
load.module("glm")
load.module("dic")
jags_example <- function(topic, renv, tenv, ...) {
    x <- sourceDcExample(topic, assign.global=FALSE)
    mod <- write.jags.model(x@model)
    t0 <- proc.time()
    out <- dcmle:::dcmle(x, ...)
    pt <- proc.time() - t0
    assign(topic, pt, envir=tenv)
    assign(topic, out, envir=renv)
    clean.jags.model(mod)
}
options(dcmle.href="c:/svn/dcr/www/examples")
n.adapt <- 100
n.update <- 100
n.iter <- 100
n.chains <- 2
thin <- 1
## this is for k=1
(topic <- listDcExamples()$topic)
timer1 <- new.env(hash=FALSE)
timer2 <- new.env(hash=FALSE)
res1 <- new.env(hash=FALSE)
res2 <- new.env(hash=FALSE)
cl <- makeSOCKcluster(3)
clusterEvalQ(cl, library(dcmle))
parLoadModule(cl, "glm")
parLoadModule(cl, "dic")
dcop <- dcoptions(verbose=0)
cat("\n\n## <<<<<<<<<<<<<<    ", date(), "    >>>>>>>>>>>>>>>>>\n\n")
for (i in topic) {
    cat("\n\n## START <<<<<<<<<<<<<<    ", i, "    >>>>>>>>>>>>>>>>>\n")
    jags_example(i, n.clones=1, renv=res1, tenv=timer1, 
        n.adapt=n.adapt, n.update=n.update, n.iter=n.iter, n.chains=n.chains, thin=thin)
    jags_example(i, n.clones=1, cl=cl, renv=res2, tenv=timer2, 
        n.adapt=n.adapt, n.update=n.update, n.iter=n.iter, n.chains=n.chains, thin=thin)
    cat("\n## END   <<<<<<<<<<<<<<    ", i, "    >>>>>>>>>>>>>>>>>\n\n")
}
stopCluster(cl)
dcoptions(dcop)
t1 <- matrix(0, length(topic), 3)
colnames(t1) <- names(timer1[[as.character(topic[1])]])[1:3]
rownames(t1) <- topic
t2 <- t1
for (i in 1:length(topic)) {
    t1[i,] <- timer1[[as.character(topic[i])]][1:3]
    t2[i,] <- timer2[[as.character(topic[i])]][1:3]
}
summary(t1)
summary(t2)
summary(t2/t1)
#setwd("c:/svn/dcr/devel/tests")
#library(dcmle)
#exampleDontRun <- function(topic) {
#    ex <- gsub("##D ", "", example(topic, "dcmle", 
#        character.only=TRUE, echo=FALSE, give.lines=TRUE))
#    f <- write.jags.model(structure(ex, class="custommodel"),
#        filename=paste(topic, ".bug", sep=""))
#    source(f, echo=TRUE)
#    clean.jags.model(f)
#    invisible(NULL)
#}
#ff <- gsub(".Rd", "", list.files("c:/svn/dcr/pkg/dcmle/man"))
#ff
#cat("\n\n## <<<<<<<<<<<<<<    ", date(), "    >>>>>>>>>>>>>>>>>\n\n")
#for (topic in ff[-2]) {
#    cat("\n\n## START <<<<<<<<<<<<<<    ", topic, "    >>>>>>>>>>>>>>>>>\n")
#    exampleDontRun(topic)
#    cat("\n## END   <<<<<<<<<<<<<<    ", topic, "    >>>>>>>>>>>>>>>>>\n\n")
#}
#cat("\n\n## START <<<<<<<<<<<<<<    endmatter    >>>>>>>>>>>>>>>>>\n")
x <- readLines("c:/svn/dcr/devel/tests/dcmle_tests.log")
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
rm(list = ls())
## EOF

DIR <- if (.Platform$OS.type == "windows")
    "c:/svn/dcr/devel" else "/home/peter/svn/dcr/devel"
setwd(paste(DIR, "/tests", sep=""))
library(dclone)
exampleDontRun <- function(topic, try_catch=TRUE) {
    ex <- gsub("##D ", "", example(topic, "dclone", 
        character.only=TRUE, echo=FALSE, give.lines=TRUE))
    f <- write.jags.model(structure(ex, class="custommodel"),
        filename=paste(topic, ".bug", sep=""))
    on.exit(clean.jags.model(f))
    out <- NULL
    if (try_catch) {
        ee <- new.env()
        ff <- try(source(f, echo=TRUE, local=ee))
        if (inherits(ff, "try-error"))
           out <- ff
    } else {
        source(f, echo=TRUE)
    }
    invisible(out)
}
ff <- if (.Platform$OS.type == "windows") {
    gsub(".Rd", "", list.files("c:/svn/dcr/pkg/dclone/man"))
} else {
    gsub(".Rd", "", list.files("/home/peter/svn/dcr/devel/tests/dclone/man"))
}
#if (.Platform$OS.type == "windows")
#    ff <- ff[!(ff %in% c("dc.fit", "bugs.fit"))] ## problems !!!
#ff <- "parCodaSamples"#"write.jags.model"
ff
res <- list()
cat("\n\n## <<<<<<<<<<<<<<    ", date(), "    >>>>>>>>>>>>>>>>>\n\n")
for (topic in ff) {
    cat("\n\n## START <<<<<<<<<<<<<<    ", topic, "    >>>>>>>>>>>>>>>>>\n")
    res[[topic]] <- exampleDontRun(topic, try_catch=FALSE)
    cat("\n## END   <<<<<<<<<<<<<<    ", topic, "    >>>>>>>>>>>>>>>>>\n\n")
}
#cat("\n\n## START <<<<<<<<<<<<<<    endmatter    >>>>>>>>>>>>>>>>>\n")
save(res, file="dclone_tests_res.Rdata")
x <- readLines(paste(DIR, "/tests/dclone_tests.log", sep=""))
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

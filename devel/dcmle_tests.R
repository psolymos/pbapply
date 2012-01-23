DIR <- if (.Platform$OS.type == "windows")
    "c:/svn/dcr/devel" else "/home/peter/svn/dcr/devel"
setwd(paste(DIR, "/tests", sep=""))
library(dcmle)
exampleDontRun <- function(topic) {
    ex <- gsub("##D ", "", example(topic, "dcmle", 
        character.only=TRUE, echo=FALSE, give.lines=TRUE))
    f <- write.jags.model(structure(ex, class="custommodel"),
        filename=paste(topic, ".bug", sep=""))
    source(f, echo=TRUE)
    clean.jags.model(f)
    invisible(NULL)
}
ff <- if (.Platform$OS.type == "windows") {
    gsub(".Rd", "", list.files("c:/svn/dcr/pkg/dcmle/man"))
} else {
    gsub(".Rd", "", list.files("/home/peter/svn/dcr/devel/tests/dcmle/man"))
}
ff
cat("\n\n## <<<<<<<<<<<<<<    ", date(), "    >>>>>>>>>>>>>>>>>\n\n")
for (topic in ff[-2]) {
    cat("\n\n## START <<<<<<<<<<<<<<    ", topic, "    >>>>>>>>>>>>>>>>>\n")
    exampleDontRun(topic)
    cat("\n## END   <<<<<<<<<<<<<<    ", topic, "    >>>>>>>>>>>>>>>>>\n\n")
}
cat("\n\n## START <<<<<<<<<<<<<<    endmatter    >>>>>>>>>>>>>>>>>\n")
x <- readLines(paste(DIR, "/tests/dcmle_tests.log", sep=""))
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

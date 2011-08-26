setwd("c:/svn/dcr/devel/tests")
library(dclone)
exampleDontRun <- function(topic) {
    ex <- gsub("##D ", "", example(topic, "dclone", 
        character.only=TRUE, echo=FALSE, give.lines=TRUE))
    f <- write.jags.model(structure(ex, class="custommodel"),
        filename=paste(topic, ".bug", sep=""))
    source(f, echo=TRUE)
    clean.jags.model(f)
    invisible(NULL)
}
ff <- gsub(".Rd", "", list.files("c:/svn/dcr/pkg/dclone/man"))
ff
cat("\n\n## <<<<<<<<<<<<<<    ", date(), "    >>>>>>>>>>>>>>>>>\n\n")
for (topic in ff) {
    cat("\n\n## START <<<<<<<<<<<<<<    ", topic, "    >>>>>>>>>>>>>>>>>\n")
    exampleDontRun(topic)
    cat("\n## END   <<<<<<<<<<<<<<    ", topic, "    >>>>>>>>>>>>>>>>>\n\n")
}
#cat("\n\n## START <<<<<<<<<<<<<<    endmatter    >>>>>>>>>>>>>>>>>\n")
x <- readLines("c:/svn/dcr/devel/tests/dctests.log")
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

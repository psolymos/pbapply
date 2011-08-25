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
if (!file.exists("c:/Program Files/WinBUGS14/")) {
    data(regmod)
    bugs.fit <- function(...) regmod
    cat("\n\n## <<<<<<<<<<<<<<    NOTE: noWinBUGS detected    >>>>>>>>>>>>>>>>>\n\n")
}
ff
cat("\n\n## <<<<<<<<<<<<<<    ", date(), "    >>>>>>>>>>>>>>>>>\n\n")
for (topic in ff) {
    cat("\n\n## START <<<<<<<<<<<<<<    ", topic, "    >>>>>>>>>>>>>>>>>\n")
    exampleDontRun(topic)
    cat("\n## END   <<<<<<<<<<<<<<    ", topic, "    >>>>>>>>>>>>>>>>>\n\n")
}

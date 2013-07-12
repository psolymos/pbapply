DIR <- if (.Platform$OS.type == "windows")
    "c:/svn/dcr/devel" else "/home/peter/svn/dcr/devel"
setwd(paste(DIR, "/tests", sep=""))

op <- options("R2WinBUGS.bugs.directory"="c:/p/WinBUGS14")

exampleDontRun <- function(topic) {
    ex <- gsub("##D ", "", example(topic, "dclone", 
        character.only=TRUE, echo=FALSE, give.lines=TRUE))
    f <- write.jags.model(structure(ex, class="custommodel"),
        filename=paste(topic, ".bug", sep=""))
    on.exit(clean.jags.model(f))
    source(f, echo=TRUE)
    invisible(NULL)
}

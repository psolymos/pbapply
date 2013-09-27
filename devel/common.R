DIR <- if (.Platform$OS.type == "windows")
    "c:/svn/dcr/devel" else "/home/peter/svn/dcr/devel"
setwd(paste(DIR, "/tests", sep=""))

op <- options("R2WinBUGS.bugs.directory"="c:/p/WinBUGS14")

topic <- "custommodel"
exampleDontRun <- function(topic) {
    example(topic, "dclone", 
        character.only=TRUE, echo=TRUE, verbose=FALSE,
        give.lines=FALSE, run.dontrun = TRUE)
}


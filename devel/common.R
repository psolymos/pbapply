DIR <- if (.Platform$OS.type == "windows")
    "c:/svn/dcr/devel" else "/home/peter/svn/dcr/devel"
setwd(paste(DIR, "/tests", sep=""))

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

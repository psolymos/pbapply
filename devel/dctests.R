date()
setwd("c:/dctests")
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
if (!file.exists("c:/Program Files/WinBUGS14/"))
    ff <- ff[!(ff %in% c("dc.fit", "dc.parfit", "bugs.fit"))]
for (topic in ff)
    exampleDontRun(topic)


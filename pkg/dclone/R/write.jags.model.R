write.jags.model <-
function(model, filename="model.bug", dir=getwd(), 
overwrite=getOption("dcoptions")$overwrite)
{
    old.dir <- getwd()
    setwd(dir)
    on.exit(setwd(old.dir))
    if (!overwrite && file.exists(filename)) {
        sn <- unlist(strsplit(filename, "\\."))
        if (length(sn) > 2) {
            sn[(length(sn) - 1)] <- paste(sn[-length(sn)], collapse=".")
            sn <- sn[-c(1:(length(sn) - 2))]
        }
        ff <- tempfile("model","")
        filename2 <- paste(substr(ff, 2, nchar(ff)), "bug", sep=".")
        if (file.exists(filename2)) {
            while (!file.exists(filename2)) {
                ff <- tempfile("model","")
                filename2 <- paste(substr(ff, 2, nchar(ff)), "bug", sep=".")
            }
        }
    } else {
        filename2 <- filename
    }
    if (inherits(model, "custommodel")) {
        writeLines(model, filename2)
    } else {
        write.model(model, filename2)
    }
    invisible(filename2)
}

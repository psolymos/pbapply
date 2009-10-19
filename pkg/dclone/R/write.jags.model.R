write.jags.model <-
function(model, filename="model.bug", dir=getwd(), n=5)
{
    require(R2WinBUGS)
    old.dir <- getwd()
    setwd(dir)
    if (file.exists(filename)) {
        sn <- unlist(strsplit(filename, "\\."))
        if (length(sn) > 2) {
            sn[(length(sn) - 1)] <- paste(sn[-length(sn)], collapse=".")
            sn <- sn[-c(1:(length(sn) - 2))]
        }
        addlet <- paste(letters[sample(1:26, n, TRUE)], collapse="")
        filename2 <- paste(sn[1], ".", addlet, ".", sn[2], sep="")
        if (file.exists(filename2)) {
            while (!file.exists(filename2)) {
                addlet <- paste(letters[sample(1:26, n, TRUE)], collapse="")
                filename2 <- paste(sn[1], ".", addlet, ".", sn[2], sep="")
            }
        }
    } else {
        filename2 <- filename
    }
    write.model(model, filename2)
    setwd(old.dir)
    invisible(filename2)
}


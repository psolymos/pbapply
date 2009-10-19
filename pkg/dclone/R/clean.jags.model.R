clean.jags.model <-
function(filename="model.bug", dir=getwd())
{
    old.dir <- getwd()
    setwd(dir)
    clean <- file.remove(filename)
    setwd(old.dir)
    invisible(clean)
}


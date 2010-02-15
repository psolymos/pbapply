clean.jags.model <-
function(filename="model.bug", dir=getwd())
{
    old.dir <- getwd()
    setwd(dir)
    on.exit(setwd(old.dir))
    clean <- file.remove(filename)
    invisible(clean)
}


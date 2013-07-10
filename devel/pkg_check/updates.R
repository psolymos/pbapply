## package dependencies for reinstalling
pkglist <- c("mefa", "mefa4", "vegan", "rgl", "mgcv", "scatterplot3d", 
    "permute", "rjags", "dclone", "dcmle", "detect", "sharx", "ade4",
    "ResourceSelection", "PVAClone", "pbapply", "coda", "snow", 
    "R2WinBUGS", "rlecuyer", "Formula", "maptools", "BRugs",
    "RODBC", "rgdal", "raster", "sp", "epiR", "plotrix",
    "reshape", "simba", "labdsv", "Hmisc", "untb", "ggplot2")

(toInst <- setdiff(pkglist, rownames(installed.packages())))

if (length(toInst) > 0)
    install.packages(toInst, repos="http://cran.at.r-project.org/")

update.packages(repos="http://cran.at.r-project.org/", ask=FALSE)

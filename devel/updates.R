## package dependencies for reinstalling
pkglist <- c("mefa", "mefa4", "vegan", "rgl", "mgcv", "scatterplot3d", 
    "permute", "rjags", "dclone", "dcmle", "detect", "sharx", "ade4",
    "ResourceSelection", "PVAClone", "pbapply", "coda", "snow", 
    "R2WinBUGS", "rlecuyer", "Formula", "maptools", "BRugs", "lme4",
    "R2OpenBUGS", "RODBC", "rgdal", "raster", "sp", "epiR", "plotrix",
    "reshape", "simba", "labdsv", "Hmisc", "untb", "ggplot2",
    "ineq", "pscl", "rpart", "gbm", "glmnet", "knitr")

(toInst <- setdiff(pkglist, rownames(installed.packages())))

if (length(toInst) > 0)
    install.packages(toInst, repos="http://cran.at.r-project.org/")

#if (.Platform$OS.type != "windows")
#    install.packages("Aspell", repos = "http://www.omegahat.org/R")

update.packages(repos="http://cran.at.r-project.org/", ask=FALSE)

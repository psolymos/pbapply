## this creates fancy model description for show/summary methods
fancyPVAmodel <- 
function(object, initial="PVA object:\n", part=1:2)
{
    fn <- object@model@fancy
    growth.model <- paste(fn[1], "growth model")
    obs.error <- if (is.na(fn[2])) {
        " without observation error" 
    } else {
        paste(" with", fn[2], "observation error")
    }
    part1 <- paste(initial, growth.model, obs.error, sep="")
    x <- object@observations
    part2 <- paste("Time series with ", length(x),
        " observations (missing: ", length(x[is.na(x)]), ")", sep="")
    if (length(part)==1 && part==1)
        return(paste(part1, sep=""))
    if (length(part)==1 && part==2)
        return(paste(part2, sep=""))
    paste(part1, "\n\n", part2, sep="")
}

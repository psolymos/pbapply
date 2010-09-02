dopb <-
function(progress.bar)
{
    if (missing(progress.bar))
        progress.bar <- getOption("pbapply.pb")
    if (!is.null(progress.bar)) {
        progress.bar <- match.arg(progress.bar, c("txt", "win", "tk", "none"))
        if (progress.bar == "none") 
            progress.bar <- NULL
    }
    interactive() && !is.null(progress.bar)
}


dopb <-
function()
{
    progress.bar <- getOption("pbapply.pb")
    if (!is.null(progress.bar)) {
        progress.bar <- match.arg(progress.bar, c("txt", "tk", "none"))
        if (progress.bar == "none") 
            progress.bar <- NULL
    }
    interactive() && !is.null(progress.bar)
}


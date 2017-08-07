pbtypes <-
function()
{
    TYPES <- c("timer", "txt", "tk", "none")
    if (.Platform$OS.type == "windows")
        c(TYPES, "win") else TYPES
}

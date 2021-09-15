pbtypes <-
function()
{
    TYPES <- c("timer", "txt", "tk", "none", "shiny")
    if (.Platform$OS.type == "windows")
        c(TYPES, "win") else TYPES
}

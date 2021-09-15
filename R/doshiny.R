doshiny <-
function()
{
    getOption("pboptions")$type == "shiny" &&
        requireNamespace("shiny") &&
        shiny::isRunning()
}

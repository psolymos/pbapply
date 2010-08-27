wait <-
function(ask = "Press any key to continue ... ", do = NULL)
{
    ANSWER <- readline(ask)
    if (!is.null(do))
        eval(do)
}

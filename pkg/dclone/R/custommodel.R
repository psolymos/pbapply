custommodel <-
function (model, exclude = NULL)
{
    if (is.function(model)) {
        model.text <- attr(model, "source")
        model.text <- sub("^\\s*function\\s*\\(\\s*\\)", "model", 
            model.text)
        model.text <- gsub("%_%", "", model.text)
    } else {
        model.text <- as.character(unlist(model))
    }
    incl <- 1:length(model.text)
    if (!is.null(exclude))
        incl <- incl[!(incl %in% exclude)]
    out <- model.text[incl]
    class(out) <- "custommodel"
    out
}


custommodel <-
function (model, exclude = NULL, digits = 5)
{
    if (is.function(model)) {
        model.text <- c("model", 
            R2WinBUGS:::replaceScientificNotationR(body(model), 
                digits=digits))
#        model.text <- attr(model, "source")
#        model.text <- sub("^\\s*function\\s*\\(\\s*\\)", "model", 
#            model.text)
        model.text <- gsub("%_%", "", model.text)
    } else {
        model.text <- as.character(unlist(model))
        if (length(model.text) < 2)
            model.text <- strsplit(model.text, "\n")[[1]]
    }
    incl <- 1:length(model.text)
    if (!is.null(exclude))
        incl <- incl[!(incl %in% exclude)]
    out <- model.text[incl]
    class(out) <- "custommodel"
    out
}


custommodel <-
function (model, exclude = NULL, digits = 5)
{

    ## not exported by R2WinBUGS, here is a copy to avoid using :::
    copy_replaceScientificNotationR <- function (bmodel, digits = 5) {
        env <- new.env()
        assign("rSNRidCounter", 0, envir = env)
        replaceID <- function(bmodel, env, digits = 5) {
            for (i in seq_along(bmodel)) {
                if (length(bmodel[[i]]) == 1) {
                    if (as.character(bmodel[[i]]) %in% c(":", "[", 
                      "[[")) 
                      return(bmodel)
                    if ((typeof(bmodel[[i]]) %in% c("double", "integer")) && 
                      ((abs(bmodel[[i]]) < 0.001) || (abs(bmodel[[i]]) > 
                        10000))) {
                      counter <- get("rSNRidCounter", envir = env) + 
                        1
                      assign("rSNRidCounter", counter, envir = env)
                      id <- paste("rSNRid", counter, sep = "")
                      assign(id, formatC(bmodel[[i]], digits = digits, 
                        format = "E"), envir = env)
                      bmodel[[i]] <- id
                    }
                }
                else {
                    bmodel[[i]] <- replaceID(bmodel[[i]], env, digits = digits)
                }
            }
            bmodel
        }
        bmodel <- deparse(replaceID(bmodel, env, digits = digits), 
            control = NULL)
        for (i in ls(env)) {
            bmodel <- gsub(paste("\"", i, "\"", sep = ""), get(i, 
                envir = env), bmodel, fixed = TRUE)
        }
        bmodel
    }

    if (is.function(model)) {
        model.text <- c("model", 
            copy_replaceScientificNotationR(body(model), 
                digits=digits))
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


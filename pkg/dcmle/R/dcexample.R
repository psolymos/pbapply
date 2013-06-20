listDcExamples <- 
function() 
{
    read.table(paste(getOption("dcmle.href"), "/index.txt", sep=""))
}

sourceDcExample <- 
function(topic, envir=parent.frame()) 
{
    dcel <- listDcExamples()
    href <- paste(getOption("dcmle.href"), "/",
        dcel[,"href"], "/", dcel[,"topic"], ".R", sep="")
    TOPIC <- as.character(dcel[,"topic"])
    if (missing(topic))
        return(TOPIC)
    e <- new.env()
    eval(parse(href[TOPIC==as.character(topic)]), e)
    if (!is.null(envir))
        assign(topic, e[[topic]], envir=envir)
    invisible(e[[as.character(topic)]])
}

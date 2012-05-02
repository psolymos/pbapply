listDcExamples <- 
function() 
{
    read.table(paste(getOption("dcmle.href"), "/index.txt", sep=""))
}

sourceDcExample <- 
function(topic, assign.global=TRUE) 
{
    dcel <- listDcExamples()
    href <- paste(getOption("dcmle.href"), "/",
        dcel[,"href"], "/", dcel[,"topic"], ".R", sep="")
    TOPIC <- as.character(dcel[,"topic"])
    if (missing(topic))
        return(TOPIC)
    e <- new.env()
    eval(parse(href[TOPIC==as.character(topic)]), e)
    if (assign.global)
        assign(topic, e[[topic]], envir=.GlobalEnv)
    invisible(e[[as.character(topic)]])
}

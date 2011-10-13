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
    TOPIC <- dcel[,"topic"]
    if (missing(topic))
        return(TOPIC)
    e <- new.env()
    eval(parse(href[TOPIC==topic]), e)
    if (assign.global)
        assign(topic, e[[topic]], envir=.GlobalEnv)
    invisible(e[[topic]])
}

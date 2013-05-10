dclone.dctr <-
function(x, n.clones = 1, attrib = TRUE, ...)
{
    rval <- t(dclone:::dclone.default(t(x), n.clones, attrib, ...))
    attr(attr(rval, "n.clones"), "method") <- "tr"
    rval
}

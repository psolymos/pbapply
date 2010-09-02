pboptions <-
function(pb, txt, gui)
{
    opb <- getOption("pbapply.pb")
    otxt <- getOption("pbapply.txt")
    ogui <- getOption("pbapply.gui")
    if (missing(pb))
        pb <- opb
    if (missing(txt))
        txt <- otxt
    if (missing(gui))
        gui <- ogui
    if (inherits(pb, "pboptions")) {
        options("pbapply.pb"=pb$pb)
        options("pbapply.txt"=pb$txt)
        options("pbapply.gui"=pb$gui)
    } else {
        npb <- opb
        npb[match(names(pb), names(npb))] <- pb
        options("pbapply.pb"=npb)
        ntxt <- otxt
        ntxt[match(names(txt), names(ntxt))] <- txt
        options("pbapply.txt"=ntxt)
        ngui <- ogui
        ngui[match(names(gui), names(ngui))] <- gui
        options("pbapply.gui"=ngui)
    }
    rval <- list(pb=opb, txt=otxt, gui=ogui)
    class(rval) <- "pboptions"
    invisible(rval)
}


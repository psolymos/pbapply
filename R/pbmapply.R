.pb_env <- new.env(parent=emptyenv())

pbmapply <-
function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
{
    ## setting up counter in .pb_env
    .pb_env$FUN <- FUN
    .pb_env$MAX <- max(sapply(list(...), length))
    .pb_env$VALUE <- 0
    .pb_env$pb <- startpb(0, .pb_env$MAX)
    on.exit(closepb(.pb_env$pb), add=TRUE)
    on.exit(rm(list=ls(envir=.pb_env), envir=.pb_env), add=TRUE)
    ## setting tracer
    suppressWarnings(suppressMessages(trace(quote(FUN),
        exit = quote({
#            .pb_env <- environment(pbmapply)$.pb_env
            .pb_env <- pbapply::.pb_env
            .pb_env$VALUE <- .pb_env$VALUE + 1
            pbapply::setpb(.pb_env$pb, .pb_env$VALUE)
        }),
#        where = environment(pbmapply)$.pb_env,
        where = .pb_env,
        print = FALSE)))
    ## piggy back on mapply
    mapply(.pb_env$FUN, ..., MoreArgs = MoreArgs,
        SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES)
}

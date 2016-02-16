closepb <-
function(pb)
{
  if(getOption("pboptions")$type == "custom")
  {
    return(invisible())
  }
  if (is.null(pb))
        invisible(NULL) else close(pb)
}


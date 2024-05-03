#'
#' @export
import_isnot0 <- function (nms, pkg = NULL) 
{
    flag_is_fn <- grepl("^is\\.|^is\\_", nms)
    stopifnot(all(flag_is_fn))
    calls <- import_fns0(nms = nms, pkg = pkg)
    out <- lapply(calls, .import_isnot0)
    new_nms <- gsub("^is[._]", "isnot_", nms)
    names(out) <- new_nms
    structure(out, class = c("isnot", "importr"))
}
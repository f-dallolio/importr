#'
#' @export
import_fns <- function (nms, pkg = NULL) 
{
    out <- import_fns0(nms = nms, pkg = pkg)
    flatten_importr(out)
}
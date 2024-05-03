#'
#' @export
import_isnot <- function (nms, pkg = NULL) 
{
    out <- import_isnot0(nms = nms, pkg = pkg)
    flatten_importr(out)
}
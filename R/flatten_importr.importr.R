#'
#' @export
flatten_importr.importr <- function (x) 
{
    if (inherits(x, "isnot")) {
        mapply(.flatten_import_isnot, x, names(x))
    }
    else {
        mapply(.flatten_import, x, names(x))
    }
}
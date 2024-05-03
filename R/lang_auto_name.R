#'
#' @export
lang_auto_name <- function (x) 
{
    if (length(x) == 1) {
        names(x) <- .lang_auto_name(x)
    }
    else {
        names(x) <- vapply(x, .lang_auto_name, character(1))
    }
    x
}
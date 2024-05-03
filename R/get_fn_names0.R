#'
#' @export
get_fn_names0 <- function (x, ..., with_ns = FALSE) 
{
    if (is.function(x)) {
        x <- list(x)
    }
    stopifnot(is.list(x) && all(vapply(x, is.function, logical(1))))
    nms <- vapply(x, .get_fn_name, character(1), with_ns = with_ns)
    nms
}
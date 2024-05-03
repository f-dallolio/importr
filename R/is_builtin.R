#'
#' @export
is_builtin <- function (x) 
{
    if (is.list(x)) {
        nms <- get_fn_names0(x)
        out <- vapply(x, .is_builtin, logical(1))
        setNames(out, nms)
    }
    else {
        .is_builtin(x)
    }
}
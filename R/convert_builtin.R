#'
#' @export
convert_builtin <- function (fn) 
{
    if (is.list(fn)) {
        nms <- get_fn_names0(fn, with_ns = FALSE)
        prim_flag <- is_builtin(fn)
        fn[prim_flag] <- lapply(fn[prim_flag], .builtin_as_closure, 
            with_ns = TRUE, auto_name = TRUE)
        setNames(fn, nms)
    }
    else {
        .builtin_as_closure(fn)
    }
}
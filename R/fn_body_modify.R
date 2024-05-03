#'
#' @export
fn_body_modify <- function (fn, value) 
{
    if (typeof(fn) == "builtin") {
        fn <- .builtin_as_closure(fn)
    }
    has_curly <- deparse(value[[1]]) == "{"
    if (has_curly) {
        body(fn) <- value
    }
    else {
        body(fn) <- call("{", value)
    }
    fn
}
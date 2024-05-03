#'
#' @export
objs_as_call <- function (..., enexpr_input = TRUE, ns_sym = TRUE, 
    str_as_expr = TRUE) 
{
    if (enexpr_input) {
        x <- rlang::enexprs(...)
    }
    else {
        x <- rlang::list2(...)
    }
    if (str_as_expr) {
        x <- purrr::modify_if(x, rlang::is_string, str2lang)
    }
    if (ns_sym) {
        fn <- function(x) rlang::is_call(x, c("::", ":::")) || 
            rlang::is_symbol(x)
    }
    else {
        fn <- function(x) rlang::is_symbol(x)
    }
    purrr::modify_if(x, fn, new_call)
}
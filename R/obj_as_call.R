#'
#' @export
obj_as_call <- function (x, enexpr_input = TRUE, ns_sym = TRUE, 
    str_as_expr = TRUE) 
{
    if (enexpr_input) {
        x <- rlang::enexpr(x)
    }
    if (str_as_expr && is_string(x)) {
        x <- str2lang(x)
    }
    if (ns_sym && is_call(x, name = c("::", ":::"))) {
        x <- new_call(x)
    }
    objs_as_call(x, enexpr_input = FALSE)[[1]]
}
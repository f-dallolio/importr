#'
#' @export
fn_match_import <- function (fn, ..., dots_env = NULL, dots_expand = TRUE) 
{
    fn <- fn_auto_name(fn, with_ns = TRUE)
    name <- names(fn)
    call <- as.call(list(str2lang(name)))
    call_match2(call = call, fn = NULL, add_ns = TRUE, fmls_syms = TRUE, 
        defaults = TRUE, dots_env = dots_env, dots_expand = dots_expand)
}
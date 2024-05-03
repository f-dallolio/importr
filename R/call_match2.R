#'
#' @export
call_match2 <- function (call = NULL, fn = NULL, ..., add_ns = FALSE, 
    fmls_syms = FALSE, defaults = TRUE, dots_env = NULL, dots_expand = TRUE) 
{
    fn_temp <- eval(call[[1]])
    if (is.primitive(fn_temp)) {
        return(call_primitive(fn_temp, with_ns = TRUE))
    }
    if (add_ns) {
        call <- call_add_ns(!!call)
    }
    if (is.null(fn)) {
        fn <- lang2fun(call)
    }
    out <- rlang::call_match(call = call, fn = fn, defaults = defaults, 
        dots_env = dots_env, dots_expand = dots_expand)
    if (fmls_syms) {
        args <- fn_fmls_syms(fn)
        args_nms <- call_args_names(out)
        call_modify(call, !!!args[args_nms])
    }
    else {
        out
    }
}
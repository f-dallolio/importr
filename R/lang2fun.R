#'
#' @export
lang2fun <- function (x, env = globalenv()) 
{
    stopifnot(is_callable(x) || is_string(x))
    if (is_symbol2(x)) {
        eval(x, envir = env)
    }
    else if (is.call(x)) {
        eval(x[[1]], envir = env)
    }
    else if (is_string(x)) {
        get(x, envir = env, mode = "function")
    }
    else if (is.function(x)) {
        x
    }
    else {
        stop("`x` must be a symbol, a call, a string, or a function.")
    }
}
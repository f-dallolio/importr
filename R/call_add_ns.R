#'
#' @export
call_add_ns <- function (call, ns = NULL, private = FALSE) 
{
    call <- obj_as_call(rlang::enexpr(call), enexpr_input = FALSE)
    if (!is.null(rlang::enexpr(ns))) {
        ns <- rlang::ensym(ns)
    }
    if (!is.call(call) || !is.symbol(call[[1]])) {
        return(call)
    }
    sym <- call[[1]]
    nm <- rlang::as_string(sym)
    if (nm %in% c("::", ":::")) {
        return(call)
    }
    if (is.null(ns)) {
        fn <- get0(nm, envir = globalenv(), mode = "function")
        stopifnot(is.function(fn))
        ns <- environmentName(environment(fn))
        op <- "::"
    }
    else {
        ns <- rlang::as_string(rlang::ensym(ns))
        if (rlang:::ns_exports_has(asNamespace(ns), nm)) {
            op <- "::"
        }
        else {
            op <- ":::"
        }
    }
    namespaced_sym <- call(op, rlang::sym(ns), sym)
    call[[1]] <- namespaced_sym
    call
}
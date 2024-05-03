#'
#' @export
fn_fmls_syms2 <- function (x) 
{
    if (is_builtin(x)) {
        x <- convert_builtin(x)
    }
    nms <- names(formals(x))
    out <- lapply(nms, str2lang)
    setNames(out, nms)
}
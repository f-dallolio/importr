#'
#' @export
fn_fmls_names2 <- function (x) 
{
    if (is_builtin(x)) {
        x <- convert_builtin(x)
    }
    names(formals(x))
}
#'
#' @export
fn_fmls2 <- function (x) 
{
    if (is_builtin(x)) {
        x <- convert_builtin(x)
    }
    formals(x)
}
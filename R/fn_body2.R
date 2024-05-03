#'
#' @export
fn_body2 <- function (x) 
{
    if (is_builtin(x)) {
        x <- convert_builtin(x)
    }
    body <- body(x)
    if (deparse(body)[[1]] == "{") {
        body[[2]]
    }
    else {
        body
    }
}
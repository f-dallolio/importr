#'
#' @export
is_symbol2 <- function (x, .ns_call = TRUE) 
{
    rlang::is_symbol(x) || is_call(x, name = c("::", ":::"))
}
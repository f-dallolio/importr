#'
#' @export
get_fn_names <- function (..., with_ns = TRUE) 
{
    x <- dots_list(..., .named = FALSE)
    get_fn_names0(x, with_ns = with_ns)
}
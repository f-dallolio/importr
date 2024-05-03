#'
#' @export
fn_auto_name <- function (..., with_ns = FALSE) 
{
    x <- dots_list(..., .named = FALSE)
    nms <- get_fn_names0(x, with_ns = with_ns)
    setNames(x, nms)
}
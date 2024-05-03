#'
#' @export
fn_auto_name0 <- function (x, ..., with_ns = FALSE) 
{
    if (is.function(x)) {
        x <- list(x)
    }
    nms <- get_fn_names0(x, with_ns = with_ns)
    setNames(x, nms)
}
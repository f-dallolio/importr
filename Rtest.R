#'
#' @export
add_export <- function (x) 
{
    lapply(x, .add_export)
}
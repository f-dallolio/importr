#'
#' @export
paste_line2 <- function (..., .before = NULL, recycle0 = FALSE) 
{
    paste_c(..., .before = .before, c = "\n", recycle0 = recycle0)
}
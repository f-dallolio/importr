#'
#' @export
paste_c <- function (..., .before = NULL, c = " ", recycle0 = FALSE) 
{
    paste(c(.before, ...), collapse = c, recycle0 = recycle0)
}
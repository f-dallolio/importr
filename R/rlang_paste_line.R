#'
#' @export
rlang_paste_line <- function (..., .trailing = FALSE) 
{
    text <- rlang::chr(...)
    if (.trailing) {
        paste0(text, "\n", collapse = "")
    }
    else {
        paste(text, collapse = "\n")
    }
}
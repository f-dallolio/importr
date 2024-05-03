#'
#' @export
cat_line <- function (..., .trailing = TRUE, file = "") 
{
    cat(rlang_paste_line(..., .trailing = .trailing), file = file)
}
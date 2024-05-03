#'
#' @export
chr2lang <- function (x, ..., .named = TRUE) 
{
    out <- lapply(x, str2lang)
    if (.named) {
        names(out) <- x
    }
    out
}
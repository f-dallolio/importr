#'
#' @export
style_importr_each <- function (x) 
{
    out <- flatten_importr(x)
    lapply(out, styler::style_text)
}
#'
#' @export
style_importr_all <- function (x) 
{
    out <- flatten_importr(x)
    styler::style_text(out)
}
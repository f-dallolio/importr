#'
#' @export
fn_call_import <- function (fn, ..., chr_out = TRUE) 
{
    out <- switch(typeof(fn), closure = fn_match_import(fn), 
        builtin = fn_primitive_import(fn), stop())
    out <- call("{", out)
    if (chr_out) {
        paste_line(out)
    }
    else {
        out
    }
}
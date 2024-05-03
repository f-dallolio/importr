#'
#' @export
fn_primitive_import <- function (fn, ..., with_ns = TRUE, auto_name = TRUE) 
{
    stopifnot(is.primitive(fn))
    name <- fn_auto_name(fn, with_ns = with_ns)
    out <- gsub(".Primitive.*", "", capture.output(is.null))
    out <- gsub(" ", "", out)
    out <- gsub("function", name, out)
    call_out <- str2lang(out)
    if (auto_name) {
        call_args_auto_name(call_out)
    }
    else {
        call_out
    }
}
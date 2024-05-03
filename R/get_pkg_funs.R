#'
#' @export
get_pkg_funs <- function (pkg, pattern = ".*", ..., full_word = FALSE, 
    negate = FALSE, internal = FALSE) 
{
    stopifnot(is.character(pkg) && length(pkg) == 1)
    envir <- asNamespace(pkg)
    if (internal) {
        nms <- names(envir)
    }
    else {
        nms <- getNamespaceExports(pkg)
    }
    if (full_word) {
        out <- nms[match(pattern, nms)]
        stopifnot(length(out) == 1)
    }
    else {
        out <- grep(pattern = pattern, x = nms, ignore.case = TRUE, 
            value = TRUE)
    }
    names(out) <- rep(pkg, length(out))
    out
}
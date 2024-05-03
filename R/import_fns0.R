#'
#' @export
import_fns0 <- function (nms, pkg = NULL) 
{
    if (rlang::is_named(nms) && is.null(pkg)) {
        lapply(unique(names(nms)), rlang::check_installed)
        pkg <- names(nms)
        names(nms) <- NULL
    }
    n <- length(nms)
    if (length(pkg) == 1) {
        pkg <- rep(pkg, n)
    }
    lapply(unique(pkg), rlang::check_installed)
    out <- mapply(.import_fn0, nms, pkg, SIMPLIFY = FALSE)
    structure(out, class = "importr")
}
#'
#' @export
fname_is_pkg_export <- function (nms, pkg) 
{
    nms %in% getNamespaceExports(pkg)
}
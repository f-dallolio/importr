#'
#' @export
call_args_auto_name <- function (call) 
{
    call_list <- as.list(call)
    args <- call_list[-1]
    new_args <- rlang::exprs_auto_name(args)
    as.call(c(call[[1]], new_args))
}
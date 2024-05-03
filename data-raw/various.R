get_pkg_funs <- function(pkg,
                         pattern = ".*",
                         ...,
                         full_word = FALSE,
                         negate = FALSE,
                         internal = FALSE) {
  stopifnot(is.character(pkg) && length(pkg) == 1)

  envir <- asNamespace(pkg)

  if (internal) {
    nms <- names(envir)
  } else {
    nms <- getNamespaceExports(pkg)
  }

  if (full_word) {
    out <- nms[match(pattern, nms)]
    stopifnot(length(out) == 1)
  } else {
    out <- grep(pattern = pattern, x = nms, ignore.case = TRUE, value = TRUE)
  }

  names(out) <- rep(pkg, length(out))

  out
}

call_args_auto_name <- function(call) {
  call_list <- as.list(call)

  args <- call_list[-1]

  new_args <- rlang::exprs_auto_name(args)

  as.call(c(call[[1]], new_args))
}

fn_primitive_import <- function(fn, ..., with_ns = TRUE, auto_name = TRUE) {
  stopifnot(is.primitive(fn))

  name <- fn_auto_name(fn, with_ns = with_ns)

  out <- gsub(".Primitive.*", "", capture.output(is.null))
  out <- gsub(" ", "", out)
  out <- gsub("function", name, out)

  call_out <- str2lang(out)

  if (auto_name) {
    call_args_auto_name(call_out)
  } else {
    call_out
  }
}

fn_match_import <- function(fn, ..., dots_env = NULL, dots_expand = TRUE) {
  fn <- fn_auto_name(fn, with_ns = TRUE)
  name <- names(fn)
  call <- as.call(list(str2lang(name)))

  call_match2(
    call = call, fn = NULL, add_ns = TRUE, fmls_syms = TRUE,
    defaults = TRUE, dots_env = dots_env, dots_expand = dots_expand
  )
}

fn_call_import <- function(fn, ..., chr_out = TRUE) {
  out <- switch(typeof(fn),
    closure = fn_match_import(fn),
    builtin = fn_primitive_import(fn),
    stop()
  )

  out <- call("{", out)

  if (chr_out) {
    paste_line(out)
  } else {
    out
  }
}

fn_body_modify <- function(fn, value) {
  if (typeof(fn) == "builtin") {
    fn <- .builtin_as_closure(fn)
  }

  has_curly <- deparse(value[[1]]) == "{"

  if (has_curly) {
    body(fn) <- value
  } else {
    body(fn) <- call("{", value)
  }

  fn
}


is_builtin <- function(x) {
  if (is.list(x)) {
    nms <- get_fn_names0(x)
    out <- vapply(x, .is_builtin, logical(1))
    setNames(out, nms)
  } else {
    .is_builtin(x)
  }
}

.is_builtin <- function(x) {
  typeof(x) == "builtin"
}



convert_builtin <- function(fn) {
  if (is.list(fn)) {
    nms <- get_fn_names0(fn, with_ns = FALSE)
    prim_flag <- is_builtin(fn)
    fn[prim_flag] <- lapply(fn[prim_flag],
      .builtin_as_closure,
      with_ns = TRUE,
      auto_name = TRUE
    )
    setNames(fn, nms)
  } else {
    .builtin_as_closure(fn)
  }
}

.builtin_as_closure <- function(fn, ..., with_ns = TRUE, auto_name = TRUE) {
  stopifnot(typeof(fn) == "builtin")
  call <- fn_primitive_import(fn = fn, with_ns = with_ns, auto_name = auto_name)
  args <- as.list(call)[-1]
  new_args <- lapply(args, \(x) base::quote(expr = ))
  fn <- as.function(c(new_args, call("{", call)))
  environment(fn) <- asNamespace("base")
  fn
}



fn_body2 <- function(x) {
  if (is_builtin(x)) {
    x <- convert_builtin(x)
  }
  body <- body(x)
  if (deparse(body)[[1]] == "{") {
    body[[2]]
  } else {
    body
  }
}

body2 <- function(x) {
  fn_body2(x)
}



fn_fmls2 <- function(x) {
  if (is_builtin(x)) {
    x <- convert_builtin(x)
  }
  formals(x)
}

formals2 <- function(x) {
  fn_fmls2(x)
}



fn_fmls_names2 <- function(x) {
  if (is_builtin(x)) {
    x <- convert_builtin(x)
  }
  names(formals(x))
}



fn_fmls_syms2 <- function(x) {
  if (is_builtin(x)) {
    x <- convert_builtin(x)
  }
  nms <- names(formals(x))
  out <- lapply(nms, str2lang)
  setNames(out, nms)
}


call_add_ns <- function (call, ns = NULL, private = FALSE){
  call <- obj_as_call(rlang::enexpr(call), enexpr_input = FALSE)

  if(!is.null(rlang::enexpr(ns))){
    ns <- rlang::ensym(ns)
  }

  if (!is.call(call) || !is.symbol(call[[1]])) {
    return(call)
  }

  sym <- call[[1]]
  nm <- rlang::as_string(sym)
  if (nm %in% c("::", ":::")) {
    return(call)
  }

  if(is.null(ns)){
    fn <- get0(nm, envir = globalenv(), mode = "function")
    stopifnot(is.function(fn))
    ns <- environmentName(environment(fn))
    op <- "::"
  } else {
    ns <- rlang::as_string(rlang::ensym(ns))
    if(rlang:::ns_exports_has(asNamespace(ns), nm)){
      op <- "::"
    } else {
      op <- ":::"
    }
  }
  namespaced_sym <- call(op, rlang::sym(ns), sym)
  call[[1]] <- namespaced_sym
  call
}


objs_as_call <- function(...,
                         enexpr_input = TRUE,
                         ns_sym = TRUE,
                         str_as_expr = TRUE){

  if(enexpr_input){
    x <- rlang::enexprs(...)
  } else {
    x <- rlang::list2(...)
  }

  if(str_as_expr){
    x <- purrr::modify_if(x, rlang::is_string, str2lang)
  }

  if(ns_sym){
    fn <- function(x) rlang::is_call(x, c("::", ":::")) || rlang::is_symbol(x)
  } else {
    fn <- function(x) rlang::is_symbol(x)
  }

  purrr::modify_if(x, fn, new_call)

}

obj_as_call <- function(x,
                        enexpr_input = TRUE,
                        ns_sym = TRUE,
                        str_as_expr = TRUE){



  if(enexpr_input){ x <- rlang::enexpr(x) }
  if(str_as_expr && is_string(x)){ x <- str2lang(x) }
  if(ns_sym && is_call(x, name = c("::", ":::"))){
    x <- new_call(x)
  }
  objs_as_call(x, enexpr_input = FALSE)[[1]]
}

chr2lang <- function(x, ..., .named = TRUE){
  out <- lapply(x, str2lang)
  if(.named){
    names(out) <- x
  }
  out
}
lang2fun <- function(x, env = globalenv()){
  stopifnot(is_callable(x) || is_string(x))
  if(is_symbol2(x)){
    eval(x, envir = env)
  } else if(is.call(x)){
    eval(x[[1]], envir = env)
  } else if(is_string(x)){
    get(x, envir = env, mode = "function")
  } else if(is.function(x)){
    x
  } else {
    stop("`x` must be a symbol, a call, a string, or a function.")
  }
}

.lang_auto_name <- function(x){
  stopifnot(typeof(x) %in% c("language", "symbol", "character"))
  if(is.character(x) && length(x) == 1){
    x
  } else {
    deparse(x)
  }
}

lang_auto_name <- function(x){
  if(length(x) == 1){
    names(x) <- .lang_auto_name(x)
  } else {
    names(x) <- vapply(x, .lang_auto_name, character(1))
  }
  x
}

is_symbol2 <- function(x, .ns_call = TRUE){
  rlang::is_symbol(x) || is_call(x, name = c("::", ":::"))
}



paste_c <- function(..., .before = NULL, c = " ", recycle0 = FALSE){
  paste(c(.before, ...), collapse = c, recycle0 = recycle0)
}
paste_flat <- function(..., .before = NULL, c = " ", recycle0 = FALSE){
  paste_c(..., .before = .before, c = c, recycle0 = recycle0)
}

rlang_paste_line <- function (..., .trailing = FALSE) {
  text <- rlang::chr(...)
  if (.trailing) {
    paste0(text, "\n", collapse = "")
  }
  else {
    paste(text, collapse = "\n")
  }
}

paste_line2 <- function(..., .before = NULL, recycle0 = FALSE){
  paste_c(..., .before = .before, c = "\n", recycle0 = recycle0)
}

cat_line <- function (..., .trailing = TRUE, file = "") {
  cat(rlang_paste_line(..., .trailing = .trailing), file = file)
}

.add_export <- function(x){
  if(!is.character(x)){
    x <- deparse(x)
  }
  c("#'\n#' @export", x)
}

add_export <- function(x){
  lapply(x, .add_export)
}


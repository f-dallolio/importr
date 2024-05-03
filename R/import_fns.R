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

  name <- .get_fn_name(fn, with_ns = with_ns)

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
  name <- .get_fn_name(fn, with_ns = TRUE)

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
    paste_flat(out, c = "\n")
  } else {
    out
  }
}

fn_body_modify <- function(fn, value) {
  if (typeof(fn) == "builtin") {
    fn <- .primitive_as_closure(fn)
  }

  has_curly <- deparse(value[[1]]) == "{"

  if (has_curly) {
    body(fn) <- value
  } else {
    body(fn) <- call("{", value)
  }

  fn
}

.import_fn <- function(nms, pkg = NULL) {
  if (is.null(pkg)) {
    finder <- find(nms, simple.words = TRUE)
    stopifnot(length(finder) != 0)
    pkg <- pkg2ns_name(rev(finder)[[1]])
  }

  fns <- get0(nms, envir = asNamespace(pkg))

  stopifnot(!is.null(fns))

  if (typeof(fns) == "builtin") {
    new_body <- fn_primitive_import(fns)

    if (fname_is_pkg_export(nms, pkg)) {
      op <- "::"
    } else {
      op <- ":::"
    }

    head <- call(op, str2lang(pkg), str2lang(nms))

    call_list <- c(head, as.list(new_body)[-1])

    new_body <- as.call(call_list)
  } else {
    new_body <- fn_match_import(fns)
  }

  new_nm <- deparse(new_body[[1]])

  fns1 <- fn_body_modify(fns, new_body)

  environment(fns1) <- asNamespace(pkg)

  fns2 <- call("<-", str2lang(nms), fns1)

  new_fns <- paste_flat(fns2, c = "\n")

  out <- sprintf("# %s -----\n%s\n#", new_nm, new_fns)

  out
}

import_fns <- function(nms, pkg = NULL) {
  n <- length(nms)

  if (length(pkg) == 1) {
    pkg <- rep(pkg, n)
  }

  lapply(unique(pkg), rlang::check_installed)

  out <- mapply(.import_fn, nms, pkg, SIMPLIFY = FALSE)

  out
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



fn_body2(x = is.null)
fn_fmls2(is.null)
fn_fmls_names2(is.null)
fn_fmls_syms2(is.null)

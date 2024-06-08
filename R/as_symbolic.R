#' Object(s) as symbolics
#'
#' @description
#' These functions take various R objects and return the equivalent symbols orcalls (e.g. `"func"` to `func`, `"pkg::func(x, ...)"` to `pkg::func(x, ...)`, etc.`).
#' * [as_symbolic_obj()] returns a symbolic.
#' * [as_symbolic_objs()] returns a list of symbolics if length of input greater than one. A symbolic otherwise.
#'
#' @inheritParams symbolic_to_str
#'
#' @param x an R object. Precisely, a syntactic literal, a symbol, a call, a function, or a formula.
#' @param ... R objects as above.
#' @param .x a list of R objects. It gets appended to `...`.
#' @param .named TRUE or FALSE. If `TRUE` (default), the unnamed elements of the output of `as_symbolic_objs` are automatically named.
#' @param .name_all TRUE or FALSE. If `TRUE` (default), all elements of the output of `as_symbolic_objs` are automatically named.
#'
#'
#' @return a symbolic object. That is, a symbol or a call.
#'
#' @examples
#' as_symbolic_obj(quote(mean))
#' as_symbolic_obj("mean")
#' as_symbolic_obj("base::mean(1:10)")
#' as_symbolic_obj(quote(sd(1:10)))
#' as_symbolic_obj(quote(base::mean))
#'
#' as_symbolic_objs(quote(mean), "mean", "base::mean(1:10)", quote(sd(1:10)))
#'
#' x <- list(quote(mean), b = "mean", c = "base::mean(1:10)", quote(sd(1:10)))
#' as_symbolic_objs("fn", .x = x)
#' as_symbolic_objs("fn", .x = x, .named = TRUE)
#' as_symbolic_objs("fn", .x = x, .named = TRUE, .name_all = TRUE)
#' as_symbolic_objs("fn", .x = x, .named = TRUE, .name_all = TRUE, .names_type = "full")
#' as_symbolic_objs("fn", .x = x, .named = TRUE, .name_all = TRUE, .names_type = "sym")
#' as_symbolic_objs("fn", .x = x, .named = TRUE, .name_all = TRUE, .names_type = "head")
#' @name as-symbolic
NULL
#'
#' @rdname as-symbolic
#' @export
as_symbolic_obj <- function(x, with_ns = TRUE){
  if(is.null(x)){
    return(NULL)
  }

  if(inherits(x, "formula")){
    out <- as.list(unclass(x))[-1]
    return(out[[1]])
  }

  if(is.function(x)){
    x <- fn_call(x, with_ns = with_ns)
  }
  if(rlang::is_string(x)){
    return(str_to_symbolic(x))
  }

  if(rlang::is_syntactic_literal(x)){
    return(as.symbol(x))
  }

  if(rlang::is_symbolic(x)){
    return(x)
  }

  cls <- class(x)
  msg <- sprintf("Cannot convert an object of class `%s` to language", cls)
  stop(msg)
}
#'
#' @rdname as-symbolic
#' @export
as_symbolic_objs <- function(...,
                        .x = NULL,
                        .named = FALSE,
                        .name_all = FALSE,
                        .names_type = c("full", "sym", "head")){
  x <- append(list(...), .x)
  out <- lapply(x, as_symbolic_obj)
  if(.named){
    symbolic_auto_name(out, .name_all = .name_all, .names_type = .names_type)
  } else {
    out
  }
}


#' Automatically name symbolic lists
#'
#' @inheritParams symbolic_to_str
#'
#' @param x a list of symbolic objects.
#' @return a named list of symbolics.
#'
#' @export
symbolic_auto_name <- function(x,
                               .name_all = FALSE,
                               .names_type = c("full", "sym", "head")){
  if(rlang::is_symbolic(x)){
    setNames(c(x), symbolic_to_str(x, .names_type = .names_type))
  } else {
    nms <- names(x)
    if(is.null(nms) || .name_all){
      names(x) <- nms <- rep("", length(x))
    }
    pos <- nms == ""
    names(x)[pos] <- symbolic_to_chr(x[pos], .names_type = .names_type)
    x
  }
}

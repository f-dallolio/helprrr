#' Get function names from functions
#'
#' @param x a function.
#' @param with_ns TRUE or FALSE. If TRUE (default), the function name(s) includes the namespace.
#' @param strict TRUE or FALSE. If FALSE (default) and `x` is not a function, the function will return `NA`. It will throw an error otherwise.
#'
#' @return a string (`fn_name`) or a character vector (`fn_names`).
#' @name function-names
NULL
#'
#' @rdname function-names
#' @export
fn_name <- function(fn, with_ns = TRUE, strict = FALSE){

  if(!is.function(fn)){
    if(strict){
      in_class <- class(x)
      msg <- sprintf('Input must be a function, not an object of class "%s"', in_class)
      stop(msg)
    } else {
      return(NA_character_)
    }
  }

  env <- environment(fn)
  if(is.null(env)){
    ns <- "base"
    env <- asNamespace(ns)
  } else {
    ns <- environmentName(env)
  }

  fn_list <- as.list(env)
  fn_flag <- vapply(fn_list, is.function, logical(1))
  fn_list <- fn_list[fn_flag]
  addrs <- lobstr::obj_addrs(fn_list)
  names(addrs) <- names(fn_list)

  addr_match <- match(lobstr::obj_addr(fn), addrs)
  fn_name <- names(addrs)[addr_match]

  if(with_ns){
    is_export <- fn_name %in% getNamespaceExports(ns)
    if(is_export){
      ns_op <- "::"
    } else {
      ns_op <- ":::"
    }
    deparse(call(ns_op, as.symbol(ns), as.symbol(fn_name)))

  } else {
    fn_name
  }
}
#'
#' @rdname function-names
#' @export
fn_names <- function(x, with_ns = TRUE, strict = FALSE){
  if(is.function(x)){
    fn_name(x, with_ns = with_ns, strict = strict)
  } else if (is.list(x)){
    vapply(x, fn_name, character(1), with_ns = with_ns, strict = strict)
  } else {
    in_class <- class(x)
    msg <- sprintf('Cannot apply `fn_names` to an object of class "%s"', in_class)
    stop(msg)
  }
}



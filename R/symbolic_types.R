#' Symbolic types
#'
#' @param x a symbol, a call, or a function.
#' @param abbr TRUE or FALSE. If TRUE, the function returns abbreviated symbolic types.
#'
#' @return a string indicating the "symbolic type" of a symbolic object.
#' @name symbolic-types
NULL
#'
#' @rdname symbolic-types
#' @export
symbolic_type0 <- function(x){
  x <- rlang::get_expr(x)
  if(typeof(x) != "language"){
    return("symbol")
  }
  if(is_ns_sym(x)){
    return("namespaced_symbol")
  }

  type <- typeof(x[[1]])
  if (type == "symbol") {
    "named_call"
  }
  else if (is_ns_call(x)) {
    "namespaced_call"
  }
  else if (type == "language") {
    "recursive_call"
  }
  else if (type %in% c("closure", "builtin", "special")) {
    "inlined_call"
  }
  else {
    stop("corrupt language object")
  }
}
#'
#' @rdname symbolic-types
#' @export
symbolic_type <- function(x, abbr = TRUE){
  type <- symbolic_type0(x)
  if(abbr){
    switch (type,
            symbol = "sym",
            namespaced_symbol = "ns_sym",
            named_call = "nm_call",
            namespaced_call = "ns_call",
            recursive_call = "rcve_call",
            inlined_call = "inln_call")
  } else {
    type
  }
}
#'
#' @rdname symbolic-types
#' @export
symbolic_types <- function(x, abbr = TRUE){
  if(!is.vector(x)){
    x <- c(x)
  }
  vapply(x, symbolic_type, character(1), abbr = abbr)
}

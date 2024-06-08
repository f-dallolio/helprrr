#' Symbolic to string or character vector
#'
#' @param x a symbol, or a call.
#' @param .names_type one of `c("full", "sym", "head")`.
#' \describe{
#'    \item{"full"}{The output is equivalent to `deparse(x)`.}
#'    \item{"sym"}{The function returns the string equivalent of a symbol `x` is a symbol or a call with no arguments.}
#'    \item{"head"}{The function returns only the string equivalent of a symbol or the call name.}
#'}
#'
#' @return a string for `symbolic_to_str` or a character vector for `symbolic_to_chr`.
#' @name symbolics-to-strings
NULL
#'
#' @rdname symbolics-to-strings
#' @export
symbolic_to_str <- function(x, .names_type = c("full", "sym", "head")){
  stopifnot("Input must be a symbol or a call." = rlang::is_symbolic(x))

  .names_type <- match.arg(.names_type)

  n <- length(as.list(x))

  if(.names_type == "full" || is_symbol2(x)){
    return(deparse(x))
  }

  if(.names_type == "head" || n == 1){
    x <- x[[1]]
  }

  deparse(x)
}
#'
#' @rdname symbolics-to-strings
#' @export
symbolic_to_chr <- function(x, .names_type = c("full", "sym", "head")){
  if(is.vector(x)){
    vapply(x, symbolic_to_str, character(1), .names_type = .names_type)
  } else {
    lang_to_str(x, .names_type = .names_type)
  }
}





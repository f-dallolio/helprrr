#' Get a call from a function
#'
#' @param x a function.
#' @param with_ns TRUE or FALSE. If TRUE (default), the call includes the namespace.
#' @param strict TRUE or FALSE. If FALSE (default) and `x` is not a function, the function will return NULL. It will throw an error otherwise.
#'
#' @return a call or a list of calls.
#' @name function-calls
NULL
#'
#' @rdname function-calls
#' @export
fn_call <- function(x, with_ns = TRUE, strict = FALSE){

  if(!is.function(x)){
    if(strict){
      in_class <- class(x)
      msg <- sprintf('Input must be a function, not an object of class "%s"', in_class)
      stop(msg)
    } else {
      return(NULL)
    }
  }

  fmls <- formals(x)
  if(is.null(fmls)) {
    fmls <- quote(x)
  } else {
    pos <- vapply(fmls, identical, logical(1), quote(expr = ))
    fmls[pos] <- lapply(names(fmls)[pos], as.symbol)
    names(fmls)[pos] <- rep("", length(fmls[pos]))
  }
  fn_nm <- fn_name(x, with_ns = with_ns, strict = strict)
  as.call(c(str2lang(fn_nm), fmls))
}
#'
#' @rdname function-calls
#' @export
fn_calls <- function(x, with_ns = TRUE, strict = FALSE){
  if(is.function(x)){
    fn_call(x, with_ns = with_ns, strict = strict)
  } else if (is.list(x)){
    lapply(x, fn_call, with_ns = with_ns, strict = strict)
  } else {
    in_class <- class(x)
    msg <- sprintf('Cannot apply `fn_calls` to an object of class "%s"', in_class)
    stop(msg)
  }
}

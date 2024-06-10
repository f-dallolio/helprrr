#' Does a character vector only contains numbers?
#'
#' @param x a character vector or an object coercible to one.
#' @param na.omit TRUE or FALSE. If TRUE (default), NAs are omitted.
#'
#' @return TRUE or FALSE.
#' @export
is_numeric_chr <- function(x, na.omit = TRUE){
  if(na.omit) x <- na.omit(x)
  if(!is.character(x)) x <- as.character(x)
  all(grepl("^[[:digit:]]+$", x))
}


#' Does a character vector only contains integers?
#'
#' @param x a character vector or an object coercible to one.
#' @param na.omit TRUE or FALSE. If TRUE (default), NAs are omitted.
#'
#' @return TRUE or FALSE.
#' @export
is_integer_chr <- function(x, na.omit = TRUE){
  if(is_numeric_chr(x, na.omit = na.omit)){
    if(na.omit) x <- na.omit(x)
    rlang::is_integerish(as.numeric(x))
  } else {
    FALSE
  }
}


#' Does a language object include namespace?
#'
#' @param x an R object.
#' @param ns a string indicating a namespace name.
#' @param private NULL, TRUE, or FALSE.
#' \describe{
#'    \item{NULL (default)}{The function returns TRUE whether the object is exported from the namespace or not.}
#'    \item{TRUE}{The function returns TRUE if it represents an object not exported from the namespace (i.e. `:::`).}
#'    \item{FALSE}{The function returns TRUE if it represents an object not exported from the namespace (i.e. `::`).}
#'}
#'
#' @return TRUE or FALSE.
#' @name namespaced-language
NULL
#'
#' @rdname namespaced-language
#' @export
is_ns_sym <- function (x, ns = NULL, private = NULL){
  if (typeof(x) != "language") {
    return(FALSE)
  }
  if (!is.null(ns) && !identical(x[[2]], str2lang(ns))) {
    return(FALSE)
  }
  head <- x[[1]]
  if (is.null(private)) {
    identical(head, quote(`::`)) || identical(head, quote(`:::`))
  } else if (private) {
    identical(head, `:::`)
  } else {
    identical(head, `::`)
  }
}
#'
#' @rdname namespaced-language
#' @export
is_ns_call <- function (x, ns = NULL, private = NULL){
  if (typeof(x) != "language") {
    return(FALSE)
  }
  if (!is_ns_sym(x[[1]], ns, private)) {
    return(FALSE)
  }
  TRUE
}



#' Extensions of rlang::is_symbol and rlang::is_call
#'
#' @inheritParams rlang::is_call
#' @param ns_sym_is_sym TRUE or FALSE. If TRUE (default), the function returns TRUE for symbols and a call of the type pkg::fun. FALSE otherwise.
#' @param ns_sym_is_call TRUE or FALSE. If TRUE (default), the function returns TRUE for a calls and a call of the type pkg::fun(). FALSE otherwise.
#'
#' @return TRUE or FALSE.
#' @name is-symbol-call-2
NULL
#'
#' @rdname is-symbol-call-2
#' @export
is_symbol2 <- function (x, name = NULL, ns_sym_is_sym = TRUE){
  if(is_ns_sym(x)){
    if(ns_sym_is_sym){
      x <- x[[3]]
    } else {
      return(FALSE)
    }
  }

  if (typeof(x) != "symbol") {
    return(FALSE)
  }

  if (is.null(name)) {
    return(TRUE)
  }

  rlang::as_string(x) %in% name
}
#'
#' @rdname is-symbol-call-2
#' @export
is_call2 <- function (x, name = NULL, n = NULL, ns = NULL, ns_sym_is_call = FALSE) {
  if(is_ns_sym(x)){
    if(ns_sym_is_call){
      x <- as.call(list(x))
    } else {
      return(FALSE)
    }
  }
  rlang::is_call(x, name = name, n = n, ns = ns)
}


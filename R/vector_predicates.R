#' Extensions of [base::all()] and [base::any()] plus their negations.
#'
#' @description
#' Extensions of [base::all()] and [base::any()] plus their negations.
#'
#'
#' @param .x an R object (ideally a list or vector).
#' @param .p a function call, a formula, or a string. It uses a simplified version of [rlang::as_function()].
#' @param ... additional arguments.
#'
#' @return TRUE or FALSE.
#' @name vectorised-predicates
NULL
#'
#' @rdname vectorised-predicates
#' @export
all2 <- function(.x, .p = identity, ...){
  if(!is.vector(.x)) .x <- list(.x)
  out <- vapply(.x, as_function(.p), logical(1), ...)
  all(out)
}
#'
#' @rdname vectorised-predicates
#' @export
any2 <- function(.x, .p = identity, ...){
  if(!is.vector(.x)) .x <- list(.x)
  out <- vapply(.x, as_function(.p), logical(1), ...)
  any(out)
}
#'
#' @rdname vectorised-predicates
#' @export
not_all <- function(.x, .p = identity, ...){
  if(!is.vector(.x)) .x <- list(.x)
  !all2(.x = .x, .p = .p, ...)
}
#'
#' @rdname vectorised-predicates
#' @export
not_any <- function(.x, .p = identity, ...){
  if(!is.vector(.x)) .x <- list(.x)
  !any2(.x = .x, .p = .p, ...)
}

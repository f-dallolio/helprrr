#' Object(s) as call(s)
#'
#' @description
#' These functions take various R objects and return the equivalent named calls (e.g. `func(x, ...), pkg::func(x, ...), etc.`).
#' * [encall()] returns a named call.
#' * [encalls()] returns a list of named calls.
#'
#'
#' @inheritParams as-symbolic
#'
#' @return A call (`encall`) or a list of calls (`encalls`).
#' @examples
#' encall(quote(mean))
#' encall("mean")
#' encall("base::mean(1:10)")
#' encall(quote(sd(1:10)))
#' encall(quote(base::mean))
#'
#' encalls(quote(mean), "mean", "base::mean(1:10)", quote(sd(1:10)))
#'
#' x <- list(quote(mean), b = "mean", c = "base::mean(1:10)", quote(sd(1:10)))
#' encalls("fn", .x = x)
#' encalls("fn", .x = x, .named = TRUE)
#' encalls("fn", .x = x, .named = TRUE, .name_all = TRUE)
#' encalls("fn", .x = x, .named = TRUE, .name_all = TRUE, .names_type = "full")
#' encalls("fn", .x = x, .named = TRUE, .name_all = TRUE, .names_type = "sym")
#' encalls("fn", .x = x, .named = TRUE, .name_all = TRUE, .names_type = "head")
#'
#' @name encall
NULL
#'
#' @rdname encall
#' @export
encall <- function(x){
  x <- as_symbolic_obj(x)
  if(is.symbol(x) || is_ns_sym(x)){
    return(as.call(list(x)))
  }
  x
}
#'
#' @rdname encall
#' @export
encalls <- function(...,
                    .x = NULL,
                    .named = FALSE,
                    .name_all = FALSE,
                    .names_type = NULL){
  x <- append(list(...), .x)
  out <- lapply(x, encall)
  if(.named){
    symbolic_auto_name(out, .name_all = .name_all, .names_type = .names_type)
  } else {
    out
  }
}

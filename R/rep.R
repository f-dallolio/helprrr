#' Helpers for base::rep
#'
#' @param x a vector (of any mode including a list) or a factor or (for rep only) a POSIXct or POSIXlt or Date object; or an S4 object containing such an object.
#' @param times an integer-valued vector giving the (non-negative) number of times to repeat each element if of length length(x), or to repeat the whole vector if of length 1. Negative or NA values are an error. A double vector is accepted, other inputs being coerced to an integer or double vector.
#' @param len non-negative numeric coercible to integer.
#'
#' @return a vector.
#' @name rep-helpers
NULL
#'
#' @rdname rep-helpers
#' @export
rep_times <- function(x, times, len = NULL){
  out <- rep.int(x, times)
  if(is.null(len)) return(out)
  stopifnot(is.numeric(len) && length(len) == 1)
  n_rep <- ceiling(len/length(out))
  rep.int(out, n_rep)[seq_len(len)]
}
#' @rdname rep-helpers
#' @export
rep_each <- function(x, times, len = NULL){
  if(length(times) == 1){
    out <- rep(x, each = times)
  } else {
    out <- rep.int(x, times = times)
  }
  if(is.null(len)) return(out)
  rep_times(out, times = 1, len = len)
}

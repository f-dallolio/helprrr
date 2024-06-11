caller_env <- function (n = 1){
  parent.frame(n = n + 1)
}

caller_arg <- function(arg){
  symbolic_to_str(substitute(arg))
}

as_function <- function(x, env = global_env()){
  if (is.function(x)) {
    return(x)
  }
  if (inherits(x, "formula")) {
    if (length(x) > 2) {
      stop("`x` must be a left-sided formula.")
    }
    args <- list(... = quote(expr = ),
                 .x = quote(..1), .y = quote(..2), . = quote(..1))
    fn <- as.function(c(args, as.list(x)[[2]]))
    return(fn)
  }
  if (is.character(x) && length(x) == 1) {
    return(get(x, envir = env, mode = "function"))
  }
  stop("Canno convert input to a function")
}



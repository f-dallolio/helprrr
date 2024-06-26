% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_fn_calls.R
\name{function-calls}
\alias{function-calls}
\alias{fn_call}
\alias{fn_calls}
\title{Get a call from a function}
\usage{
fn_call(x, with_ns = TRUE, strict = FALSE)

fn_calls(x, with_ns = TRUE, strict = FALSE)
}
\arguments{
\item{x}{a function.}

\item{with_ns}{TRUE or FALSE. If TRUE (default), the call includes the namespace.}

\item{strict}{TRUE or FALSE. If FALSE (default) and \code{x} is not a function, the function will return NULL. It will throw an error otherwise.}
}
\value{
a call or a list of calls.
}
\description{
Get a call from a function
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subs.R
\name{subs}
\alias{subs}
\title{Substitute symbol for value}
\usage{
subs(sym, nms, vls)
}
\arguments{
\item{sym}{Expression}

\item{nms}{Names of symbols (see Details)}

\item{vls}{Values that \code{nms} is substituted with (see Details)}
}
\description{
Substitute symbol for value
}
\details{
Two different ways to call this function is supported:
\enumerate{
\item Supplying \code{nms} as a named list and omitting \code{vls}.
If two components have the same name, the behaviour is undefined.
\item Supplying both \code{nms} and \code{vls}
See Examples.
}
}
\examples{
if (has_sympy()) {
   x <- symbol('x')
   e <- 2*x^2
   e
   subs(e, "x", "2")
   subs(e, x, 2)
   subs(e, list(x = 2))
   
   A <- matrix_sym(2, 2, "a")
   B <- matrix_sym(2, 2, "b")
   e <- A \%*\% A
   subs(e, A, B)
}

}
\concept{caracas_symbol}

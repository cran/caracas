% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sym_symbol.R
\name{unbracket}
\alias{unbracket}
\title{Remove inner-most dimension}
\usage{
unbracket(x)
}
\arguments{
\item{x}{Array symbol to collapse dimension from}
}
\description{
Remove inner-most dimension
}
\examples{
if (has_sympy()) {
  x <- as_sym(paste0("x", 1:3))
  y <- as_sym("y")
  l <- list(x, y)
  l
  unbracket(l)
}

}
\concept{caracas_symbol}

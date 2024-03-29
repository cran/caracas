% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coerce.R
\name{matrify}
\alias{matrify}
\title{Creates matrix from array symbol}
\usage{
matrify(x)
}
\arguments{
\item{x}{Array symbol to convert to matrix}
}
\description{
Creates matrix from array symbol
}
\examples{
if (has_sympy()) {
  x <- symbol("x")
  y <- symbol("y")
  f <- 3*x^2 + x*y^2
  matrify(f)
  h <- der2(f, list(x, y))
  h
  dim(h)
  H <- matrify(h)
  H
  dim(H)
}

}
\concept{caracas_symbol}

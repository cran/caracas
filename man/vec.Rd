% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lin-alg.R
\name{vec}
\alias{vec}
\title{Stacks matrix to vector}
\usage{
vec(x)
}
\arguments{
\item{x}{Matrix}
}
\description{
Stacks matrix to vector
}
\examples{
if (has_sympy()) {
  A <- as_sym(matrix(1:9, 3))
  vec(A)
}

}
\concept{linalg}

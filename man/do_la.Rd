% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lin-alg-advanced.R
\name{do_la}
\alias{do_la}
\title{Do linear algebra operation}
\usage{
do_la(x, slot, ...)
}
\arguments{
\item{x}{A matrix for which a property is requested}

\item{slot}{The property requested}

\item{...}{Auxillary arguments}
}
\value{
Returns the requested property of a matrix.
}
\description{
Do linear algebra operation
}
\examples{
if (has_sympy()) {
  A <- matrix(c("a", "0", "0", "1"), 2, 2) \%>\% as_sym()
  
  do_la(A, "QR")
  QRdecomposition(A)
  
  do_la(A, "eigenval")
  eigenval(A)
  
  do_la(A, "eigenvec")
  eigenvec(A)
  
  do_la(A, "inv")
  inv(A)
  
  do_la(A, "echelon_form")
  do_la(A, "rank")
  
  do_la(A, "det") # Determinant
  det(A)
}

}
\concept{linalg}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/symbol.R
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
  x <- as_sym("[[[x1/(b2 + x1)], 
                 [x2/(b2 + x2)], 
                 [x3/(b2 + x3)]], 
                [[-b1*x1/(b2 + x1)^2], 
                 [-b1*x2/(b2 + x2)^2], 
                 [-b1*x3/(b2 + x3)^2]]]")
  x
  unbracket(x)
  
  x <- as_sym("Matrix([[b1*x1/(b2 + x1)], [b1*x2/(b2 + x2)], [b1*x3/(b2 + x3)]])")
  
}

}
\concept{caracas_symbol}

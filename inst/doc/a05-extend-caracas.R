## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE-----------------------------------------------------------
library(caracas)

## ---- include = FALSE---------------------------------------------------------
inline_code <- function(x) {
  x
}

if (!has_sympy()) {
  # SymPy not available, so the chunks shall not be evaluated
  knitr::opts_chunk$set(eval = FALSE)
  
  inline_code <- function(x) {
    deparse(substitute(x))
  }
}

## -----------------------------------------------------------------------------
cofactor_matrix <- function(x) {
  sympy_func(x, "cofactor_matrix")
}

cofactor <- function(x, i, j) {
  # Python indexing starts at 0 - thus subtract 1 to convert from R indexing
  # to Python indexing
  sympy_func(x, "cofactor", i - 1, j - 1)
}

## -----------------------------------------------------------------------------
A <- matrix_sym(3, 3, "a")

## -----------------------------------------------------------------------------
CC <- cofactor_matrix(A)
CC
cc <- cofactor(A, 1, 1)
cc

## -----------------------------------------------------------------------------
B <- t(CC) / det(A)
P <- A %*% B
P %>% simplify()


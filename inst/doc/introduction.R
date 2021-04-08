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
x <- symbol('x')
eq <- 2*x^2 - x
eq
as.character(eq)
as_expr(eq)
tex(eq)

## -----------------------------------------------------------------------------
solve_sys(eq, x)
der(eq, x)
subs(eq, x, "y")

## -----------------------------------------------------------------------------
A <- matrix(c("x", 2, 0, "2*x"), 2, 2)
B <- as_sym(A)
B
Binv <- inv(B) # or solve_lin(B)
Binv
tex(Binv)

## -----------------------------------------------------------------------------
eigenval(Binv)
eigenvec(Binv)


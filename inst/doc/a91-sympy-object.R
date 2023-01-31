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
sympy <- get_sympy()

## -----------------------------------------------------------------------------
sympy$diff("2*a*x", "x")
sympy$solve("x**2 - 1", "x")

## -----------------------------------------------------------------------------
d <- sympy$symbols('d')
h <- sympy$symbols('h')

## -----------------------------------------------------------------------------
lam <- sympy$symbols('lam')

## -----------------------------------------------------------------------------
area_str <- "Pi/2 * d**2 + Pi * h * d"
vol_str <- "Pi/4 * d**2 * h"
lap_str <- paste0("(", area_str, ") - lam*((", vol_str, ") - 1)")
lap <- sympy$parsing$sympy_parser$parse_expr(
  lap_str,
  local_dict = list('d' = d, 'h' = h, 'lam' = lam))

## -----------------------------------------------------------------------------
grad <- sympy$derive_by_array(lap, list(d, h, lam))
grad

## -----------------------------------------------------------------------------
sol <- sympy$solve(grad, list(d, h, lam), dict = TRUE)
sol

## -----------------------------------------------------------------------------
sol[[1]]

## -----------------------------------------------------------------------------
to_r <- function(x) {
  x <- as.character(x)
  x <- gsub("Pi", "pi", x, fixed = TRUE)
  x <- gsub("**", "^", x, fixed = TRUE)
  x <- parse(text = x)
  return(x)
}

sol_d <- to_r(sol[[1]]$d)
sol_d
eval(sol_d)
sol_h <- to_r(sol[[1]]$h)
sol_h
eval(sol_h)

## -----------------------------------------------------------------------------
x <- sympy$symbols('x')
x$assumptions0
x <- sympy$symbols('x', positive = TRUE)
x$assumptions0
eq <- sympy$parsing$sympy_parser$parse_expr("x**2 - 1",
                                            local_dict = list('x' = x))
sympy$solve(eq, x, dict = TRUE)

## -----------------------------------------------------------------------------
x <- sympy$symbols('x', positive = TRUE)
eq <- sympy$parsing$sympy_parser$parse_expr("x**3/3 - x",
                                            local_dict = list('x' = x))
eq
grad <- sympy$derive_by_array(eq, x)
grad
sympy$solve(grad, x, dict = TRUE)


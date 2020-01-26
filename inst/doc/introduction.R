## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(caracas)

## ---- include = FALSE---------------------------------------------------------
if (!have_sympy()) {
  # SymPy not available, so the chunks shall not be evaluated
  knitr::opts_chunk$set(eval = FALSE)
}

## -----------------------------------------------------------------------------
sympy <- get_sympy()

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


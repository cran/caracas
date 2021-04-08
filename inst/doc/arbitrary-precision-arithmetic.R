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
n_2 <- as_sym("2")
n_pi <- as_sym("pi", declare_symbols = FALSE) # pi is a constant in SymPy
x <- sqrt(n_2) * n_pi
x
N(x)
N(x, 5)
N(x, 50)
as.character(N(x, 50))

## -----------------------------------------------------------------------------
mpmath <- reticulate::import('mpmath')
mpmath$mp$dps <- 30
z <- mpmath$mpc(real = '1.0', imag = '63.453')
zeta_z <- mpmath$zeta(z)
zeta_z
as.character(zeta_z$real)
as.character(zeta_z$imag)


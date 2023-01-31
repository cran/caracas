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

## ----ar1, echo=F--------------------------------------------------------------
N <- 3
L1 <- diag(4)
L1[cbind(1 + (1:N), 1:N)] <- "-a"
L1 <- as_sym(L1)

## ---- echo=F------------------------------------------------------------------
e <- as_sym(paste0("e", 0:3))
x <- as_sym(paste0("x", 0:3))
u <- as_sym(paste0("u", 1:3))
y <- as_sym(paste0("y", 1:3))
eu <- rbind(e, u)
xy <- rbind(x, y)

## -----------------------------------------------------------------------------
N <- 3
L1 <- diag(4)
L1[cbind(1 + (1:N), 1:N)] <- "-a"
L1 <- as_sym(L1)

## -----------------------------------------------------------------------------
def_sym(v2)
L1inv <- inv(L1)
V1 <- v2 * L1inv %*% t(L1inv)
K1 <- (t(L1) %*% L1) / v2

## ---- echo=F, results="asis"--------------------------------------------------
cat(
  "\\begin{align} 
    K_1 &= ", tex(K1), " \\\\ 
   V_1 &= ", tex(V1), " 
  \\end{align}", sep = "")

## ----L2, echo=F---------------------------------------------------------------
N <- 3
L2 <- diag("1", 1 + 2*N)
L2[cbind(1 + (1:N), 1:N)] <- "-a"
L2[cbind(1 + N + (1:N), 1 + 1:N)] <- "-b"
L2 <- as_sym(L2)

## -----------------------------------------------------------------------------
N <- 3
L2 <- diag("1", 1 + 2*N)
L2[cbind(1 + (1:N), 1:N)] <- "-a"
L2[cbind(1 + N + (1:N), 1 + 1:N)] <- "-b"
L2 <- as_sym(L2)

## -----------------------------------------------------------------------------
Veu <- diag(1, 7)
diag(Veu)[1:4] <- "v2"
diag(Veu)[5:7] <- "w2"
Veu
Veu <- as_sym(Veu)
Veu
L2inv <- inv(L2) 
V2 <- L2inv %*% Veu %*% t(L2inv) 
K2 <- t(L2) %*% inv(Veu) %*% L2

## ---- results="asis", echo=F--------------------------------------------------
cat(
  "\\begin{align} K_2 &= ", tex(K2), " \\\\ 
                  V_2 &= ", tex(V2), " \\end{align}", sep = "")


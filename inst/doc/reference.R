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

if (!have_sympy()) {
  # SymPy not available, so the chunks shall not be evaluated
  knitr::opts_chunk$set(eval = FALSE)
  
  inline_code <- inline_code <- function(x) {
    deparse(substitute(x))
  }
}

## -----------------------------------------------------------------------------
x <- symbol('x')
as.character(x)
x
as_r(x)

## -----------------------------------------------------------------------------
2*x
y <- symbol('y')
sqrt(3*x^y)

## -----------------------------------------------------------------------------
z <- cos(x)^2 + sin(x)^2
z
simplify(z)
tex(z)

## -----------------------------------------------------------------------------
z <- cos(x)*cos(y) - sin(x)*sin(y)
z
simplify(z)
z <- cos(x + y)
z
expand(z)
expand_trig(z)

## -----------------------------------------------------------------------------
x <- symbol('x')
y <- symbol('y')
z <- log(x*y)
z
expand_log(z)

## -----------------------------------------------------------------------------
x <- symbol("x")
sumf(1/x, "x", 1, 10)
sumf(1/x, x, 1, 10)
s <- sumf(1/x, "x", 1, 10)
as_r(s)
sum(1/(1:10))
n <- symbol("n")
simplify(sumf(x, x, 1, n))

## -----------------------------------------------------------------------------
x <- symbol("x")
p <- prodf(1/x, "x", 1, 10)
p
as_r(p)
prod(1/(1:10))
n <- symbol("n")
prodf(x, x, 1, n)

## -----------------------------------------------------------------------------
x <- symbol("x")

intf(1/x, x, 1, 10)
i1 <- intf(1/x, x, 1, 10, doit = FALSE)
i1
tex(i1)
doit(i1)
intf(1/x, x)
i1 <- intf(1/x, x, doit = FALSE)
i1
tex(i1)
doit(i1)

## -----------------------------------------------------------------------------
x <- symbol("x")
limf(sin(x)/x, "x", 0)
limf(1/x, "x", 0, dir = '+')
limf(1/x, "x", 0, dir = '-')

## -----------------------------------------------------------------------------
x <- symbol("x")
limf(sin(x)/x, "x", 0)
limf(sin(x)/x, x, 0)

## -----------------------------------------------------------------------------
res <- limf(sin(x)/x, "x", 0, doit = FALSE)
res
as.character(res)
tex(res)
doit(res)
as_r(res)

## -----------------------------------------------------------------------------
x <- symbol("x")
y <- symbol("y")
f <- 3*x^2 + x*y^2
f
as_r(f)
der(f, "x")
der(f, x)
der(f, c("x", "y"))
der(f, c(x, y))
f1 <- der(f, c(x, y))
f1
as.character(f1)
as_r(f1)
eval(as_r(f1), list(x = 1, y = 2))
der(f1, c(x, y))
f2 <- der2(f, c(x, y))
f2
as_r(f2)
eval(as_r(f2), list(x = 1, y = 2))

## -----------------------------------------------------------------------------
x <- symbol("x")
y <- symbol("y")
f <- eval_to_symbol("[3*x**2 + x*y**2, 2*x, 5*y]")
f
der(f, c(x, y))

## -----------------------------------------------------------------------------
A <- matrix(c("x", 0, 0, "2*x"), 2, 2)
A
B <- as_symbol(A)
B
2*B
B*B # Component-wise / Hadamard product
dim(B)
sqrt(B)
log(B)
sum(B)
B %*% t(B)
diag(B)
cbind(B, B)
rbind(B, B)

## -----------------------------------------------------------------------------
determinant(B)

## -----------------------------------------------------------------------------
A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
B <- as_symbol(A)
eigen_val(B)
eigen_vec(B)
eigen(eval(as_r(B), list(a = 2)))

## -----------------------------------------------------------------------------
B
diag(B)
diag(B) <- "b"
B
diag(B)[-2] <- "a"
B

## -----------------------------------------------------------------------------
p <- as_symbol(paste0("p", 1:3))
y <- as_symbol(paste0("y", 1:3))
a <- as_symbol("a")
l <- sum(y*log(p))
l
L <- -l + a*(sum(p) - 1)
L
tex(L)
g <- der(L, c(p, a))
g
sol <- solve_sys(g, c(p, a))
sol
sol[[1L]]$p1
tex(sol[[1L]]$p1)

## -----------------------------------------------------------------------------
x <- symbol('x')
eq <- 2*x^2 - x
eq
subs(eq, x, "y")

## -----------------------------------------------------------------------------
p <- as_symbol(paste0("p", 1:3))
y <- as_symbol(paste0("y", 1:3))
a <- as_symbol("a")
l <- sum(y*log(p))
L <- -l + a*(sum(p) - 1)
g <- der(L, c(a, p))
sols <- solve_sys(g, c(a, p))
sol <- sols[[1L]]
sol
H <- der2(L, c(p, a))
H
H_sol <- subs_lst(H, sol)
H_sol

## -----------------------------------------------------------------------------
A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
B <- as_symbol(A)
B[, 2]
B[, -2]
B[1, ]
B[1, , drop = FALSE] # Note this is a 1x3 matrix
B[, 2] <- "x"
B

## -----------------------------------------------------------------------------
sympy <- get_sympy()

## -----------------------------------------------------------------------------
sympy$diff("2*a*x", "x")
sympy$solve("x**2 - 1", "x")

## -----------------------------------------------------------------------------
# Multinomial likelihood
p <- as_symbol(paste0("p", 1:3))
y <- as_symbol(paste0("y", 1:3))
a <- as_symbol("a")
l <- sum(y*log(p))
L <- -l + a*(sum(p) - 1)
L
print(L, ascii = TRUE)
g <- der(L, c(p, a))
sol <- solve_sys(g, c(p, a))
sol
print(sol, simplify = FALSE)

## -----------------------------------------------------------------------------
as.character(g)
as_character_matrix(g)

## -----------------------------------------------------------------------------
sol
L
options(caracas.print.prettyascii = TRUE) 
sol
L
options(caracas.print.prettyascii = NULL) # reset to default (FALSE)

## -----------------------------------------------------------------------------
sol
L
options(caracas.print.ascii = TRUE) 
sol
L
options(caracas.print.ascii = NULL) # reset to default (FALSE)

## -----------------------------------------------------------------------------
p
options(caracas.print.rowvec = FALSE)
p
options(caracas.print.rowvec = NULL) # reset to default (TRUE)

## -----------------------------------------------------------------------------
sol
options(caracas.print.sol.simplify = FALSE)
sol
options(caracas.print.sol.simplify = NULL) # reset to default (TRUE)


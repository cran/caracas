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
symbol("a")

## -----------------------------------------------------------------------------
b <- symbol("a")
a <- symbol("b")

## -----------------------------------------------------------------------------
a + 1
a <- a + 1
a / b

## -----------------------------------------------------------------------------
a %>% print.default()
a %>% as.character()

## -----------------------------------------------------------------------------
def_sym(u, v)
def_sym("w", "x")
def_sym_vec(c("y", "z"))

## -----------------------------------------------------------------------------
u; v; w; x; y; z

## -----------------------------------------------------------------------------
as_sym("l1")
# same as symbol("l1")
l2 <- as_sym("l2"); l2
# same as def_sym("l2")

## -----------------------------------------------------------------------------
m_ <- paste0("m", 1:4)
m <- as_sym(m_)
m

B_ <- matrix(c("x", 2, 0, "2*x"), 2, 2)
B <- as_sym(B_)

## -----------------------------------------------------------------------------
m %>% symbol_class()
u %>% symbol_class()

## -----------------------------------------------------------------------------
m %>% as.character()
u %>% as.character()

## -----------------------------------------------------------------------------
u %>% to_list()
u %>% to_vector()
m %>% to_list()
m %>% to_vector()

## -----------------------------------------------------------------------------
u %>% to_list() %>% as.character()
u %>% to_vector() %>% as.character()
m %>% to_list() %>% as.character()
m %>% to_vector() %>% as.character()

## -----------------------------------------------------------------------------
m %>% to_matrix()
u %>% to_matrix()

## -----------------------------------------------------------------------------
v <- m %>% to_vector()
l <- m %>% to_list()
V <- matrix_sym(2, 2)

## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
def_sym('x', 'y')
eq <- 2*x^2 - x - y
eq
as.character(eq)
as_expr(eq)
tex(eq)

## -----------------------------------------------------------------------------
sol <- solve_sys(eq, x)
sol
# Access solutions
sol[[1]]$x
sol[[2]]$x

dx <- der(eq, x)
dx
dx %>% symbol_class()

dxy <- der(eq, c(x, y))
dxy
dxy %>% symbol_class()

subs(eq, x, y)

## -----------------------------------------------------------------------------
B_ <- matrix(c("x", 2, 0, "2*x"), 2, 2)
B <- as_sym(B_)
B
Binv <- inv(B) # or solve_lin(B)
Binv
tex(Binv)
det(B)
Binv * det(B)


## -----------------------------------------------------------------------------
eigenval(Binv)
eigenvec(Binv)


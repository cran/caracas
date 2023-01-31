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

## ---- echo = FALSE, fig.width=8, fig.height=5---------------------------------
old_mar <- par("mar")
par(mar = c(0, 0, 0, 0))
A <- 300
kat <- sqrt(1000^2 - A^2)

pA <- c(0, A)
pB <- c(0, 0)
pC <- c(kat, 0)

D <- 400
pD <- c(D, 0)

pad <- 40
plot(0, type = 'n', axes = FALSE, ann = FALSE, 
     xlim = c(-pad, 1000+pad), 
     ylim = c(-pad, A+pad))

polygon(c(pA[1]-pad, pB[1]-pad, pC[1]+pad, pC[1]+pad), 
        c(pA[2]+pad, pB[2]-pad, pC[2]-pad, pA[2]+pad), border = NA, col = "green")

points(pA[1], pA[2], type = "p"); text(pA[1], pA[2], labels = "A", pos = 2)
points(pB[1], pB[2], type = "p"); text(pB[1], pB[2], labels = "B", pos = 2)
points(pC[1], pC[2], type = "p"); text(pC[1], pC[2], labels = "C", pos = 4)
points(pD[1], pD[2], type = "p"); text(pD[1], pD[2], labels = "D", pos = 1)

lines(c(pA[1], pB[1]), c(pA[2], pB[2]), type = "l", col = "black", lty = 2)
lines(c(pA[1], pC[1]), c(pA[2], pC[2]), type = "l", col = "black", lty = 2)
lines(c(pB[1], pC[1]), c(pB[2], pC[2]), type = "l", col = "black", lwd = 3)
lines(c(pA[1], pD[1]), c(pA[2], pD[2]), type = "l", col = "black", lty = 2)

legend(1000-200, A, 
       legend = c("5 m/s", "2 m/s"), 
       lty = c(1, 2), 
       lwd = c(3, 1))
text(-pad/3, A/2, labels = "|AB| = 300 m", pos = 3, srt = 90)
text(kat/2, A/1.7, labels = "|AC| = 1,000 m", pos = 3)

par(mar = old_mar)

## -----------------------------------------------------------------------------
AB <- as_sym('300')
AB
AC <- as_sym('1000')
AC
BC <- sqrt(AC^2 - AB^2)
BC
k <- symbol('k') # |BD|
DC <- BC - k
AD <- sqrt(AB^2 + k^2)
AD

## -----------------------------------------------------------------------------
l <- AD/2 + DC/5
l

## ---- fig.width=7, fig.height=5-----------------------------------------------
lfun <- as_expr(l)
lfun
ks <- seq(0, as_expr(AC), length.out = 100)
ls <- eval(lfun, list(k = ks))
plot(ks, ls, type = "l", xlab = "k", ylab = "Time A to C")

## -----------------------------------------------------------------------------
dl <- der(l, k)
dl
crit_points <- solve_sys(dl, k)
crit_points
best_k <- crit_points[[1]]$k
best_k

## -----------------------------------------------------------------------------
eval(as_expr(der(dl, k)), list(k = as_expr(best_k)))

## -----------------------------------------------------------------------------
DC_best <- BC - best_k
AD_best <- sqrt(AB^2 + best_k^2)
AD_best
best_route <- AD_best + DC_best
best_route
as_expr(best_route)

## -----------------------------------------------------------------------------
best_l <- subs(l, "k", best_k)
best_l
as_expr(best_l)

## ---- echo = FALSE, fig.width=8, fig.height=5---------------------------------
old_mar <- par("mar")
par(mar = c(0, 0, 0, 0))
A <- 300
kat <- sqrt(1000^2 - A^2)

pA <- c(0, A)
pB <- c(0, 0)
pC <- c(kat, 0)

D <- as_expr(best_k)
pD <- c(D, 0)

pad <- 40
plot(0, type = 'n', axes = FALSE, ann = FALSE, 
     xlim = c(-pad, 1000+pad), 
     ylim = c(-pad, A+pad))

polygon(c(pA[1]-pad, pB[1]-pad, pC[1]+pad, pC[1]+pad), 
        c(pA[2]+pad, pB[2]-pad, pC[2]-pad, pA[2]+pad), border = NA, col = "green")

points(pA[1], pA[2], type = "p"); text(pA[1], pA[2], labels = "A", pos = 2)
points(pB[1], pB[2], type = "p"); text(pB[1], pB[2], labels = "B", pos = 2)
points(pC[1], pC[2], type = "p"); text(pC[1], pC[2], labels = "C", pos = 4)
points(pD[1], pD[2], type = "p"); text(pD[1], pD[2], labels = "D", pos = 1)

lines(c(pA[1], pB[1]), c(pA[2], pB[2]), type = "l", col = "black", lty = 2)
lines(c(pA[1], pC[1]), c(pA[2], pC[2]), type = "l", col = "black", lty = 2)
lines(c(pB[1], pC[1]), c(pB[2], pC[2]), type = "l", col = "black", lwd = 3)
lines(c(pA[1], pD[1]), c(pA[2], pD[2]), type = "l", col = "black", lty = 2)

lines(c(pA[1], pD[1]), c(pA[2], pD[2]), type = "l", col = "red", lwd = 2.5)
lines(c(pD[1], pC[1]), c(pD[2], pC[2]), type = "l", col = "red", lwd = 2.5)

legend(1000-200, A, 
       legend = c("5 m/s", "2 m/s"), 
       lty = c(1, 2), 
       lwd = c(3, 1))
text(-pad/3, A/2, labels = "|AB| = 300 m", pos = 3, srt = 90)
text(kat/2, A/1.7, labels = "|AC| = 1,000 m", pos = 3)

par(mar = old_mar)


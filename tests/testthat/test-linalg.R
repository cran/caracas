context("linalg")

test_that("Math", {
  skip_if_no_sympy()
  
  A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
  B <- as_sym(A)

  expect_equal(as.character(B), "Matrix([[a, 0, a], [0, a, 0], [0, a, 0]])")
  expect_equal(as.character(2*B), "Matrix([[2*a, 0, 2*a], [0, 2*a, 0], [0, 2*a, 0]])")
  expect_equal(as.character(B + B), "Matrix([[2*a, 0, 2*a], [0, 2*a, 0], [0, 2*a, 0]])")
  expect_equal(as.character(B - B), "Matrix([[0, 0, 0], [0, 0, 0], [0, 0, 0]])")
  expect_equal(as.character(B*B), "Matrix([[a^2, 0, a^2], [0, a^2, 0], [0, a^2, 0]])")
})




test_that("determinant", {
  skip_if_no_sympy()
  
  B <- as_sym("[[x, 1], [2, x**2]]")
  
  expect_equal(as.character(det(B)), "x^3 - 2")
})

test_that("reciprocal_matrix", {
  skip_if_no_sympy()

  B <- as_sym("[[x, a], [a, x**2]]")
  Bchar1 <- as.character(reciprocal_matrix(B))
  Bchar2 <- as.character(reciprocal_matrix(B, num=2))
  
  expect_equal(Bchar1, "Matrix([[1/x, 1/a], [1/a, x^(-2)]])")
  expect_equal(Bchar2, "Matrix([[2/x, 2/a], [2/a, 2/x^2]])")
  
  
  expect_equal(as.character(reciprocal_matrix(as_sym("Matrix([[a, c], [b, 1]])"))),
               "Matrix([[1/a, 1/c], [1/b, 1]])")
  expect_equal(as.character(reciprocal_matrix(as_sym("Matrix([[a, c], [b, 1]])"), "a")),
               "Matrix([[1, a/c], [a/b, a]])")
})




test_that("diag", {
  skip_if_no_sympy()
  
  B <- as_sym("[[x, 1], [2, x**2]]")
  expect_equal(as.character(diag(B)), "Matrix([[x, x^2]])")
  
  A <- matrix(c("a", 4, 2, 1, "a", "a"), 2, 3)
  B <- as_sym(A)
  expect_equal(as.character(diag(B)), "Matrix([[a, 1]])")
})



test_that("eigenvalues and eigenvectors", {
  skip_if_no_sympy()
  
  A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
  B <- as_sym(A)
  
  eval <- eigenval(B)
  eval_order <- order(unlist(lapply(eval, function(l) l$eigmult)))
  eval <- eval[eval_order]
  
  expect_equal(as.character(eval[[1L]]$eigval), "0")
  expect_equal(eval[[1L]]$eigmult, 1L)
  expect_equal(as.character(eval[[2L]]$eigval), "a")
  expect_equal(eval[[2L]]$eigmult, 2L)
  
  
  evec <- eigenvec(B)
  evec_order <- order(unlist(lapply(evec, function(l) l$eigmult)))
  evec <- evec[evec_order]
  
  expect_equal(as.character(evec[[1L]]$eigval), "0")
  expect_equal(evec[[1L]]$eigmult, 1L)
  expect_equal(as.character(evec[[1L]]$eigvec), "Matrix([[-1], [0], [1]])")
  expect_equal(as.character(evec[[2L]]$eigval), "a")
  expect_equal(evec[[2L]]$eigmult, 2L)
  expect_equal(as.character(evec[[2L]]$eigvec), "Matrix([[1], [0], [0]])")
})


test_that("eigenvalues and eigenvectors 2", {
  skip_if_no_sympy()
  
  A <- matrix(c("x", 2, 0, "2*x"), 2, 2)
  B <- as_sym(A)
  Binv <- inv(B) 
  
  eval <- eigenval(Binv)
  eval_order <- order(unlist(lapply(eval, function(l) as.character(l$eigval))))
  eval <- eval[eval_order]
  
  expect_equal(as.character(eval[[1L]]$eigval), "1/(2*x)")
  expect_equal(eval[[1L]]$eigmult, 1L)
  expect_equal(as.character(eval[[2L]]$eigval), "1/x")
  expect_equal(eval[[2L]]$eigmult, 1L)
  
  
  
  evec <- eigenvec(Binv)
  expect_equal(length(evec), 2L)
  
  evec_order <- order(unlist(lapply(evec, function(l) as.character(l$eigval))))
  evec <- evec[evec_order]
  
  expect_equal(as.character(evec[[1L]]$eigval), "1/(2*x)")
  expect_equal(evec[[1L]]$eigmult, 1L)
  expect_equal(as.character(evec[[1L]]$eigvec), "Matrix([[0], [1]])")
  expect_equal(as.character(evec[[2L]]$eigval), "1/x")
  expect_equal(evec[[2L]]$eigmult, 1L)
  expect_equal(as.character(evec[[2L]]$eigvec), "Matrix([[-x/2], [1]])")
})


test_that("as_character_matrix", {
  skip_if_no_sympy()
  
  x <- as_sym(1)
  expect_equal(as_character_matrix(x), "1")
  
  
  b <- as_sym(1:3)
  
  expect_equal(as_character_matrix(b), 
               structure(c("1", "2", "3"), .Dim = c(3L, 1L)))
  
  expect_equal(as_character_matrix(t(b)), 
               structure(c("1", "2", "3"), .Dim = c(1L, 3L)))
})


test_that("do_la", {
  skip_if_no_sympy()
  
  A <- matrix(c("2", "0", "0", "1"), 2, 2) %>% as_sym()
  
  
  res <- QRdecomposition(A)
  expect_equal(as.character(res$Q), "Matrix([[1, 0], [0, 1]])")
  expect_equal(as.character(res$R), "Matrix([[2, 0], [0, 1]])")
  
  
  res <- inv(A)
  expect_equal(as.character(res), "Matrix([[1/2, 0], [0, 1]])")

  res <- det(A)
  expect_equal(as.character(res), "2")

  res <- eigenval(A)
  expect_equal(as.character(res[[1]]$eigval), "2")
  expect_equal(res[[1]]$eigmult, 1)
  expect_equal(as.character(res[[2]]$eigval), "1")
  expect_equal(res[[2]]$eigmult, 1)
  
  
  p <- do_la(A, "charpoly")
  expect_equal(as.character(p), "lambda^2 - 3*lambda + 2")
  expect_equal(as.character(as_expr(p)), "lambda^2 - 3 * lambda + 2")
  
  expect_equal(as_expr(do_la(A, "rank")), 2L)
  
  expect_equal(as_expr(do_la(A, "cofactor", 0, 1)), 0L)
  
  expect_equal(as.character(do_la(A, "echelon_form")), "Matrix([[2, 0], [0, 1]])")
  
  
  B <- as_sym("[[9, 3*I], [-3*I, 5]]")
  expect_equal(as_expr(do_la(B, "cholesky")), 
               structure(c(3+0i, 0-1i, 0+0i, 2+0i), .Dim = c(2L, 2L)))
  
  B <- t(as_sym("[[ 2, 3, 5 ], [3, 6, 2], [8, 3, 6]]"))
  expect_equal(as.character(do_la(B, "GramSchmidt")), 
               "Matrix([[2, 23/19, 1692/353], [3, 63/19, -1551/706], [5, -47/19, -423/706]])")
  
  B_rref <- do_la(B, "rref")
  expect_equal(as.character(B_rref$mat), 
               "Matrix([[1, 0, 0], [0, 1, 0], [0, 0, 1]])")
  expect_equal(B_rref$pivot_vars, c(1L, 2L, 3L))
  
})

  
test_that("diag_", {
  skip_if_no_sympy()
  
  expect_equal(as.character(diag_(c("a", "b", "c"))), 
               "Matrix([[a, 0, 0], [0, b, 0], [0, 0, c]])")
  
  expect_equal(as.character(diag_("a", 2)), "Matrix([[a, 0], [0, a]])")
})

test_that("matrix_", {
  skip_if_no_sympy()
  
  expect_equal(as.character(matrix_(1:9, nrow = 3)), 
               "Matrix([[1, 4, 7], [2, 5, 8], [3, 6, 9]])")
  expect_equal(as.character(matrix_("a", 2, 2)), "Matrix([[a, a], [a, a]])")
})


test_that("mat_pow", {
  skip_if_no_sympy()
  
  M <- matrix_(c("1", "a", "a", 1), 2, 2)
  
  if ("pow" %in% names(M$pyobj)) {
    Msqrt <- mat_pow(M, 1/2)
    
    expect_equal(as.character(Msqrt), 
                 "Matrix([[sqrt(1 - a)/2 + sqrt(a + 1)/2, -sqrt(1 - a)/2 + sqrt(a + 1)/2], [-sqrt(1 - a)/2 + sqrt(a + 1)/2, sqrt(1 - a)/2 + sqrt(a + 1)/2]])")
  } else {
    expect_error(mat_pow(M, 1/2))
  }
  
})


test_that("sym constructors", {
  skip_if_no_sympy()
  
  n <- 4
  m <- 3
  expect_equal(as.character(vector_sym(n, "v")),
               "Matrix([[v1], [v2], [v3], [v4]])")
  
  expect_equal(as.character(matrix_sym(n, m, "v")),
               "Matrix([[v11, v12, v13], [v21, v22, v23], [v31, v32, v33], [v41, v42, v43]])")
  
  expect_equal(as.character(matrix_sym_diag(n, "v")),
               "Matrix([[v1, 0, 0, 0], [0, v2, 0, 0], [0, 0, v3, 0], [0, 0, 0, v4]])")
  
  expect_equal(as.character(matrix_sym_symmetric(n, "v")),
               "Matrix([[v11, v21, v31, v41], [v21, v22, v32, v42], [v31, v32, v33, v43], [v41, v42, v43, v44]])")
  
  
})

test_that("colspan", {
  skip_if_no_sympy()
  
  X <- matrix_(paste0("x_",c(1,1,1,1,2,2,2,2,3,4,3,4)), nrow = 4)
  expect_equal(as_character_matrix(colspan(X)), 
               structure(c("x_1", "x_1", "x_1", "x_1", "x_3", "x_4", "x_3", "x_4"), 
                         dim = c(4L, 2L)))
})

test_that("rankMatrix", {
  skip_if_no_sympy()
  
  X <- matrix_(paste0("x_",c(1,1,1,1,2,2,2,2,3,4,3,4)), nrow=4)
  res <- rankMatrix_(X)
  expect_equal(as.numeric(res), 2)
})

test_that("add_prefix", {
  skip_if_no_sympy()
  
  X <- matrix_sym(2, 3)
  Y <- add_prefix(X, "e")
  expect_equal(dim(X), dim(Y))
  expect_equal(as.character(Y), "Matrix([[ev11, ev12, ev13], [ev21, ev22, ev23]])")
  
  X <- matrix(1:6, 3, 2)
  Y <- add_prefix(X, "e")
  expect_equal(dim(X), dim(Y))
  expect_equal(as.character(Y), "Matrix([[e1, e4], [e2, e5], [e3, e6]])")
  
})

# test_that("kronecker", {
#   skip_if_no_sympy()
#   
#   # FIXME: Consider removing test 
#   # 
#   # # Needed for test to run
#   # setOldClass("caracas_symbol")
#   # 
#   # A <- matrix_sym(2, 2, "a")
#   # B <- matrix_sym(2, 2, "b")
#   # II <- matrix_sym_diag(2)
#   # EE <- eye_sym(2, 2)
#   # JJ <- ones_sym(2, 2)
#   # 
#   # expect_equal(as.character(kronecker(A, B)), "Matrix([[a11*b11, a11*b12, a12*b11, a12*b12], [a11*b21, a11*b22, a12*b21, a12*b22], [a21*b11, a21*b12, a22*b11, a22*b12], [a21*b21, a21*b22, a22*b21, a22*b22]])")
#   # expect_equal(as.character(kronecker(A, B, FUN = "+")), "Matrix([[a11 + b11, a11 + b12, a12 + b11, a12 + b12], [a11 + b21, a11 + b22, a12 + b21, a12 + b22], [a21 + b11, a21 + b12, a22 + b11, a22 + b12], [a21 + b21, a21 + b22, a22 + b21, a22 + b22]])")
#   # expect_equal(as.character(kronecker(II, B)), "Matrix([[b11*v1, b12*v1, 0, 0], [b21*v1, b22*v1, 0, 0], [0, 0, b11*v2, b12*v2], [0, 0, b21*v2, b22*v2]])")
#   # expect_equal(as.character(kronecker(EE, B)), "Matrix([[b11, b12, 0, 0], [b21, b22, 0, 0], [0, 0, b11, b12], [0, 0, b21, b22]])")
#   # expect_equal(as.character(kronecker(JJ, B)), "Matrix([[b11, b12, b11, b12], [b21, b22, b21, b22], [b11, b12, b11, b12], [b21, b22, b21, b22]])")
#   # 
#   # ####
#   # 
#   # A <- matrix(c(2, 4, 5, 8, 2, 7), nrow = 3)
#   # B <- matrix(c(6, 3, 2, 2, 2, 7, 6, 8, 4, 6, 6, 4), nrow = 4)
#   # K <- kronecker(A, B)
#   # K2 <- kronecker(as_sym(A), as_sym(B))
#   # expect_true(inherits(K2, "caracas_symbol"))
#   # expect_equal(K, as_expr(K2))
# })


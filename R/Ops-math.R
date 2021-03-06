get_pyobj <- function(e, method) {
  o <- if (method == "") {
    reticulate::py_eval(as.character(e), convert = FALSE)
  } else {
    e$pyobj
  }
  
  return(o)
}



matrix_ele_power <- function(x, power = 1){
  ensure_sympy()
  
  if (!symbol_is_matrix(x)) {
    stop("'x' must be sympy matrix\n")
  }
  
  power <- as.character(power)
  
  if (grepl("^-?[0-9]+$", power)) {
    # S(): Sympify
    # Means that this will be e.g. (S(1))/(1) = 1 instead of 1/1 = 1.0 (numeric)
    power <- paste0("S(", power, ")")
  }
  
  rx <- apply(as_character_matrix(x), 2, function(xx) {
    paste0("(", xx, ")**(", power, ")")
  })
  
  return(as_sym(rx))
}


#' Math operators
#'
#' @param e1 A `caracas_symbol`.
#' @param e2 A `caracas_symbol`.
#'
#' @concept simple_algebra
#' 
#' @export
Ops.caracas_symbol = function(e1, e2) {
  ensure_sympy()

  if (!(.Generic %in% c("+", "-", "*", "/", "^"))) {
    stop("Function '", .Generic, "' not yet implemented for caracas_symbol")
  }
  
  # E.g. -4 etc...
  if (missing(e2)) {
    o1 <- get_pyobj(e1, .Method[1L])
    txt <- paste0(.Generic, "(", as.character(o1), ")")
    s <- eval_to_symbol(txt)
    return(s)
  }

  # LHS may be constant, e.g. 2*x: e1 is a number 2
  o1 <- get_pyobj(e1, .Method[1L])
  
  # RHS may be constant, e.g. x*2: e2 is a number 2
  o2 <- get_pyobj(e2, .Method[2L])
  
  op <- if (.Generic == "^") {
    "**"
  } else {
    .Generic
  }
  
  e1_is_mat <- symbol_is_matrix(as.character(o1))
  e2_is_mat <- symbol_is_matrix(as.character(o2))

  ## str(list(o1=o1, o2=o2))
  ## str(list(e1_is_mat=e1_is_mat, e2_is_mat=e2_is_mat))
  
  if (e1_is_mat && e2_is_mat) {
    dim_e1 <- dim(e1)
    ##cat("mat && mat\n")    
    if (isTRUE(all.equal(dim_e1, dim(e2))) && any(dim_e1 == 1L)) {
      # Component wise operation
      if (.Generic == "*") {
        z <- get_sympy()$matrix_multiply_elementwise(o1, o2)
        return(construct_symbol_from_pyobj(z))
      }
      else if (.Generic == "/"){ ## FIXME Mikkel; check this, please
        e2 <- reciprocal_matrix(e2)
        o2 <- e2$pyobj
        ##print(o1)
        ##print(o2)
        z <- get_sympy()$matrix_multiply_elementwise(o1, o2)
        return(construct_symbol_from_pyobj(z))
      }
    }
  }
  
  
  if (.Generic %in% c("+", "-", "*", "/")) {
    if (e1_is_mat && !e2_is_mat) {
      ##cat("mat || !mat\n")
      if (.Generic == "/") {
        e2 <- as_sym(scalar_to_matrix(paste0("1/(", as.character(e2), ")"), dim(e1)))
        o2 <- e2$pyobj
        e2_is_mat  <- TRUE
        .Generic <- "*"
      } else {
        e2 <- as_sym(scalar_to_matrix(as.character(e2), dim(e1)))
        o2 <- e2$pyobj
        e2_is_mat  <- TRUE
      }
      
      # e2 <- as_sym(scalar_to_matrix(as.character(e2), dim(e1)))
      # o2 <- e2$pyobj
    } else if (!e1_is_mat && e2_is_mat) {
      ###cat("!mat && mat\n")
      e1 <- as_sym(scalar_to_matrix(as.character(e1), dim(e2)))
      o1 <- e1$pyobj
      e1_is_mat  <- TRUE
    } 
  }

  ### cat("HERE\n"); str(list(o1=o1, o2=o2)); str(list(e1_is_mat=e1_is_mat, e2_is_mat=e2_is_mat))
  
  # Component-wise * / ^ for matrices
  # +/- is handled with normal operators
  if ((e1_is_mat || e2_is_mat) && .Generic %in% c("*", "/", "^")) {
    ### cat("mat || mat\n")
    if (.Generic == "*") {
      z <- get_sympy()$matrix_multiply_elementwise(o1, o2)
      w <- construct_symbol_from_pyobj(z)
      return(w) 
    } else if (.Generic == "/") {
      e2 <- reciprocal_matrix(e2)
      o2 <- e2$pyobj
      z <- get_sympy()$matrix_multiply_elementwise(o1, o2)
      w <- construct_symbol_from_pyobj(z)
      return(w)
    } else if (.Generic == "^") {
      #print(e1)
      #print(e2)
      w <- matrix_ele_power(e1, e2)
      return(w) 
    } else {
      stop("Unexpected")
    }
  }

  cmd <- paste0("(", as.character(o1), ")", op, 
                "(", as.character(o2), ")")


  x <- eval_to_symbol(cmd)
  return(x)
}







  ## if (e1_is_mat && e2_is_mat) {
  ##   dim_e1 <- dim(e1)
    
  ##   if (isTRUE(all.equal(dim_e1, dim(e2))) && any(dim_e1 == 1L)) {
  ##     # Component wise operation
  ##     if (.Generic == "*") {
  ##       z <- get_sympy()$matrix_multiply_elementwise(o1, o2)
  ##       cat("we are done...\n")
  ##       return(construct_symbol_from_pyobj(z))
  ##     }
  ##   } else if (!(.Generic %in% c("+", "-", "*"))) {
  ##     stop("Only +, -, * (component-wise) and %*% are valid for matrix-matrix/matrix-vector operations")
  ##   }
  ## }
    ## else if (!(.Generic %in% c("+", "-"))) {
    ##   stop("Only +, -, * (component-wise) and %*% are valid for matrix-matrix/matrix-vector operations")
    ## }


Math_transtab <- matrix( c(
  #R					Python
  "sin",			"Sin",
  "cos",			"Cos",
  "tan",			"Tan",

  "asin",	  	"ArcSin",
  "acos",	  	"ArcCos",
  "atan",    	"ArcTan",
  "asinh", 	  "ArcSinh",
  "acosh", 	  "ArcCosh",
  "atanh",   	"ArcTanh",

  "exp", 	  	"Exp",
  "log", 	  	"Ln",
  "sqrt", 	  "Sqrt",
  
  "gamma",    "gamma"
), byrow = TRUE, ncol = 2)
colnames(Math_transtab) <- c("R", "Python")
#paste0(Math_transtab[, 1], "()", collapse = ", ")

#' Math functions
#' 
#' If `x` is a matrix, the function is applied component-wise.
#'
#' @param x `caracas_symbol`.
#' @param \dots further arguments passed to methods
#'
#' @concept simple_algebra
#' 
#' @export
Math.caracas_symbol = function(x, ...) {
  ensure_sympy()

  i <- match(.Generic, Math_transtab[, 1L])

  if (is.na(i)) {
    stop("Function '", .Generic, "' not yet implemented for caracas_symbol")
  }

  fn <- Math_transtab[i, 1L]
  
  sympy <- get_sympy()

  if (is.null(sympy[fn])) {
    stop(paste0("Could not find function '", fn, "' in Python"))
  }

  if (symbol_is_matrix(x)) {
    y <- x$pyobj$applyfunc(sympy[fn])
    z <- construct_symbol_from_pyobj(y)
    return(z)
  }
  
  y <- sympy[fn](x$pyobj)
  z <- construct_symbol_from_pyobj(y)

  return(z)
}


#' Matrix multiplication
#'
#' @param x Object `x`
#' @param y Object `y`
#' 
#' @name matrix-products
#' @seealso [base::%*%()]
#' 
#' @concept linalg
#' 
#' @export
`%*%` <- function(x, y) {
  UseMethod("%*%")
}

#' @export
`%*%.default` <- function(x, y) {
  return(base::`%*%`(x, y))
}

#' Matrix multiplication
#' 
#' @param x Object `x`
#' @param y Object `y`
#' 
#' @name matrix-products
#' @seealso [base::%*%()]
#' 
#' @concept linalg
#' 
#' @export
`%*%.caracas_symbol` <- function(x, y) {
  ensure_sympy()
  
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  if (!inherits(y, "caracas_symbol")) {
    stop(paste0("'y' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  if (inherits(x$pyobj, "sympy.matrices.expressions.matexpr.MatrixExpr") || 
      inherits(y$pyobj, "sympy.matrices.expressions.matexpr.MatrixExpr")) {
    
    s <- get_sympy()
    y <- s$MatMul(x$pyobj$doit(), y$pyobj$doit())$doit()
    y <- y$as_explicit()
    z <- construct_symbol_from_pyobj(y)
    return(z)
  }
  
  z <- paste0("(", as.character(x$pyobj), ") * (", as.character(y$pyobj), ")")
  y <- eval_to_symbol(z)
  
  return(y)
}


#' Define caracas symbols in global environment
#' 
#' @seealso [symbol()], [as_sym()]
#' 
#' @param ... Names for new symbols, also supports non-standard evaluation
#' @param charvec Take each element in this character vector and define as caracas symbols
#' @param warn Warn if existing variable names are overwritten
#' @param env Environment to assign variable in
#' 
#' @return Names of declared variables (invisibly)
#' 
#' @examples 
#' if (has_sympy()) {
#'   ls()
#'   def_sym(n1, n2, n3)
#'   ls()
#'   def_sym("x1", "x2", "x3")
#'   ls()
#'   def_sym("x1", "x2", "x3", warn = TRUE)
#'   ls()
#'   def_sym(i, j, charvec = c("x", "y"))
#'   ls()
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
def_sym <- function(..., 
                    charvec = NULL, 
                    warn = FALSE,
                    env = parent.frame()) {
  
  ensure_sympy()
  
  dots <- match.call(expand.dots = FALSE)$...
  
  nms <- lapply(seq_along(dots), function(i) {
    y <- if (is.symbol(dots[[i]])) {
      deparse(dots[[i]])
    } else {
      dots[[i]]
    }
    
    return(y)
  })
  
  if (!is.null(charvec) && is.character(charvec)) {
    nms <- c(nms, charvec)
  }
  
  existing_nms <- c()
  
  for (i in seq_along(nms)) {
    v <- nms[[i]]
    
    verify_variable_name(v)
    
    if (warn && exists(v, envir = env)) {
      existing_nms <- c(existing_nms, v)
    }
    
    cmd <- paste0(v, " = symbols('", v, "')")
    # py_run_string instead of py_eval because we need to assign inside Python
    s <- reticulate::py_run_string(cmd, convert = FALSE)
    res <- s[[v]]

    y <- construct_symbol_from_pyobj(res)
    
    assign(v, value = y, envir = env)
  }
  
  if (warn && length(existing_nms) > 0) {
    warning("The following names were overwritten: ", paste0(nms, collapse = ", "))
  }
  
  return(invisible(nms))
}

#' @export
"print.summary.ldBounds" <- function(x, digit = 5, ...)
{
  z <- x
  if (!inherits(z, "summary.ldBounds")) 
    stop("'x' must inherit from class \"summary.ldBounds\"")
  rownames(z$bounds) <- rownames(z$bounds, do.NULL = FALSE, prefix = "")
  if (z$type%in%(1:3)){
    cat("\nLan-DeMets bounds for a given spending function \n", "\nn = ", z$n, "\nOverall alpha: ", z$oalpha, "\n")
  }
  if (z$type%in%(4:6)){
    cat("\nGroup sequential boundaries  \n", "\nn = ", z$n, "\nOverall alpha: ", z$oalpha, "\n")
  }
  if (z$type%in%c(1,4)){
    if (z$type==1){
      cat("\nType: One-Sided Bounds", "\nalpha: ", z$alpha, "\nSpending function:", z$spending, "\n", "\nBoundaries:\n")
    }
    if (z$type==4){
      cat("\nType: One-Sided Bounds", "\nalpha: ", z$alpha, "\nBoundary type (non-alpha-spending):", z$spending, "\n", "\nBoundaries:\n")
    }
    if (ncol(z$bounds)==6) 
      print.default(z$bounds[,-2], digits = 5, quote = FALSE, print.gap = 2)
    else
      print.default(z$bounds[,-3], digits = 5, quote = FALSE, print.gap = 2)
  }
  else{
    if (z$type==2){
      if (length(z$alpha)==2){
        cat("\nType: Two-Sided Symmetric Bounds", "\nLower alpha: ", z$alpha[1], "\nUpper alpha: ", z$alpha[1], "\nSpending function: ", z$spending, "\n")
      }
      else{
        cat("\nType: Two-Sided Symmetric Bounds", "\nLower alpha: ", z$alpha/2, "\nUpper alpha: ", z$alpha/2, "\nSpending function: ", z$spending, "\n")
      }
    }                                               
    if (z$type==5){
      if (length(z$alpha)==2){
        cat("\nType: Two-Sided Symmetric Bounds", "\nLower alpha: ", z$alpha[1], "\nUpper alpha: ", z$alpha[1], "\nBoundary type (non-alpha-spending): ", z$spending, "\n")
      }
      else{
        cat("\nType: Two-Sided Symmetric Bounds", "\nLower alpha: ", z$alpha/2, "\nUpper alpha: ", z$alpha/2, "\nBoundary type (non-alpha-spending): ", z$spending, "\n")
      }
    }                                               
    if (z$type==3){
      cat("\nType: Two-Sided Asymmetric Bounds", "\nLower alpha: ", z$alpha[1], "\nSpending function for the lower boundary: ", z$spending[1], "\nUpper alpha: ", z$alpha[2], "\nSpending function for the upper boundary: ", z$spending[2], "\n")
    }
    if (z$type==6){
      cat("\nType: Two-Sided Asymmetric Bounds", "\nLower alpha: ", z$alpha[1], "\nType of (non-alpha-spending) lower boundary: ", z$spending[1], "\nUpper alpha: ", z$alpha[2], "\nType of (non-alpha-spending) upper boundary: ", z$spending[2], "\n")
    }
    cat("\nBoundaries:\n")
    print.default(z$bounds, digits = digit, quote = FALSE, print.gap = 2)
  }
}

#' @export
"print.ldBounds" <- function(x, ...)
{
  z <- x
  if (!inherits(z, "ldBounds"))
    stop("'x' must inherit from class \"ldBounds\"")
  p <- length(z$time)
  if (identical(z$time,z$time2)){
    b <- matrix(NA, p, 3)
    b[,1:3] <- c(z$time, z$lower.bounds, z$upper.bounds) 
    colnames(b) <- c("Time", "Lower", "Upper")
  }
  else{
    b <- matrix(NA, p, 4)
    b[,1:4] <- c(z$time, z$time2, z$lower.bounds, z$upper.bounds) 
    colnames(b) <- c("Time", "Time 2", "Lower", "Upper")
  }
  ans <- list()
  ans$type <- z$bounds.type
  ans$spending <- z$spending.type
  ans$n <- p
  ans$alpha <- z$alpha    
  ans$oalpha <- z$overall.alpha
  ans$bounds <- b
  rownames(ans$bounds) <- rownames(ans$bounds, do.NULL = FALSE, prefix = "")
  if (ans$type%in%(1:3)){
    cat("\nLan-DeMets bounds for a given spending function \n", "\nn = ", ans$n, "\nOverall alpha: ", ans$oalpha, "\n")
  }
  if (ans$type%in%(4:6)){
    cat("\nGroup sequential boundaries  \n", "\nn = ", ans$n, "\nOverall alpha: ", ans$oalpha, "\n")
  }
  if (ans$type%in%c(1,4)){
    if (ans$type==1){
      cat("\nType: One-Sided Bounds", "\nalpha: ", ans$alpha, "\nSpending function:", ans$spending, "\n", "\nBoundaries:\n")
    }
    if (ans$type==4){
      cat("\nType: One-Sided Bounds", "\nalpha: ", ans$alpha, "\nBoundary type (non-alpha-spending):", ans$spending, "\n", "\nBoundaries:\n")
    }
    if (ncol(ans$bounds)==3) 
      print.default(ans$bounds[,-2], digits = 5, quote = FALSE, print.gap = 2, ...)
    else
      print.default(ans$bounds[,-3], digits = 5, quote = FALSE, print.gap = 2, ...)
    cat("\n")
  }
  else{
    if (ans$type==2){
      if (length(ans$alpha)==2){
        cat("\nType: Two-Sided Symmetric Bounds", "\nLower alpha: ", ans$alpha[1], "\nUpper alpha: ", ans$alpha[2], "\nSpending function: ", ans$spending, "\n")
      }
      else{
        cat("\nType: Two-Sided Symmetric Bounds", "\nLower alpha: ", ans$alpha/2, "\nUpper alpha: ", ans$alpha/2, "\nSpending function: ", ans$spending, "\n")
      }            
    }                                               
    if (ans$type==5){
      if (length(ans$alpha)==2){
        cat("\nType: Two-Sided Symmetric Bounds", "\nLower alpha: ", ans$alpha[1], "\nUpper alpha: ", ans$alpha[1], "\nBoundary type (non-alpha-spending): ", ans$spending, "\n")
      }
      else{
        cat("\nType: Two-Sided Symmetric Bounds", "\nLower alpha: ", ans$alpha/2, "\nUpper alpha: ", ans$alpha/2, "\nBoundary type (non-alpha-spending): ", ans$spending, "\n")
      }
    }                                               
    if (ans$type==3){
      cat("\nType: Two-Sided Asymmetric Bounds", "\nLower alpha: ", ans$alpha[1], "\nSpending function for the lower boundary: ", ans$spending[1], "\nUpper alpha: ", ans$alpha[2], "\nSpending function for the upper boundary: ", ans$spending[2], "\n")
    }
    if (ans$type==6){
      cat("\nType: Two-Sided Asymmetric Bounds", "\nLower alpha: ", ans$alpha[1], "\nType of (non-alpha-spending) lower boundary: ", ans$spending[1], "\nUpper alpha: ", ans$alpha[2], "\nType of (non-alpha-spending) upper boundary: ", ans$spending[2], "\n")
    }
    cat("\nBoundaries:\n")
    print.default(ans$bounds, quote = FALSE, print.gap = 2, ...)
    cat("\n")
  }
}

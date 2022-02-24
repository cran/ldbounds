#' @export
"summary.ldBounds" <- function (object, digit=5, ...) 
{
  z <- object
  if (!inherits(z, "ldBounds")) 
    stop("'object' must inherit from class \"ldBounds\"")
  p <- length(z$time)
  if (identical(z$time,z$time2)){
    b <- matrix(NA, p, 6)
    b[,1:6] <- c(z$time, z$lower.bounds, z$upper.bounds, z$exit.pr, z$diff.pr, z$nom.alpha) 
    colnames(b) <- c("Time", "Lower", "Upper", "Exit pr.", "Diff. pr.", "Nominal Alpha")
  }
  else{
    b <- cbind(z$time, z$time2, z$lower.bounds, z$upper.bounds, z$exit.pr, z$diff.pr, z$nom.alpha)
    colnames(b) <- c("Time", "Time 2", "Lower", "Upper", "Exit pr.", "Diff. pr.", "Nominal Alpha")
  }
  if (is.na(b[1,ncol(b)])) b <- b[,-ncol(b)]
  ans <- list()
  ans$type <- z$bounds.type
  ans$spending <- z$spending.type
  ans$n <- p
  ans$alpha <- z$alpha    
  ans$oalpha <- z$overall.alpha
  ans$bounds <- b
  class(ans) <- "summary.ldBounds"
  return(ans)
}

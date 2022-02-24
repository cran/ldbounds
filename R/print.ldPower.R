#' @export
"print.ldPower" <- function(x, digit = 5, ...)
{
  z <- x
  if (!inherits(z, "ldPower")) 
    stop("'x' must inherit from class \"ldPower\"")
  ans <- list()
  ans$type <- z$type
  ans$n <- length(z$time)
  if ((ans$type==1)|(ans$type==2)){
    ans$power <- z$power
    ans$drift <- z$drift
    if (identical(z$time,z$time2)){           
      b <- matrix(NA, ans$n, 3)
      b[,1:3] <- c(z$time, z$lower.probs, z$upper.probs)
      colnames(b) <- c("Time", "Lower probs", "Upper probs")  
      ans$bounds1 <- b
    }
    else{           
      b <- matrix(NA, ans$n, 4)
      b[,1:4] <- c(z$time, z$time2, z$lower.probs, z$upper.probs)
      colnames(b) <- c("Time", "Time 2", "Lower probs", "Upper probs")
      ans$bounds1 <- b
    }   
  }     
  if (ans$type==3){
    ans$level <- z$conf.level
    ans$fzvalue <- z$final.zvalue
    ans$interval <- z$conf.interval        
  }
  if (ans$type==3){
    ans$level <- z$conf.level
    ans$fzvalue <- z$final.zvalue
    ans$interval <- z$conf.interval        
  }
  if (ans$type==4){
    if (z$p.ordering=="SW"){
      ans$p.ordering <- "Stage-wise"
    }
    if (z$p.ordering=="LR"){
      ans$p.ordering <- "Likelihood ratio "
    }
    ans$fzvalue <- z$final.zvalue
    ans$analysis.time <- z$analysis.time
    ans$p.value <- z$p.value        
  }
  if (identical(z$time,z$time2)){
    ans$bounds <- matrix(c(z$time, z$lower.bounds, z$upper.bounds), ncol=3, dimnames = list(NULL,c("Time", "Lower", "Upper")))
  }   
  else{
    ans$bounds <- matrix(c(z$time, z$time2, z$lower.bounds, z$upper.bounds), ncol=4, dimnames = list(NULL,c("Time", "Time 2", "Lower", "Upper"))) 
  }
  rownames(ans$bounds) <- rownames(ans$bounds, do.NULL = FALSE, prefix = "")
  cat("\nLan-DeMets method for group sequential boundaries \n", "\nn = ", ans$n, "\n")
  cat("\nBoundaries: \n") 
  if ((ans$type==1)|(ans$type==2)){
    rownames(ans$bounds1) <- rownames(ans$bounds1, do.NULL = FALSE, prefix = "")
    print.default(cbind(ans$bounds,ans$bounds1[,-1]), quote = FALSE, print.gap = 2, ...)
    cat("\nPower : ", ans$power, "\n","\nDrift: ", ans$drift, "\n\n")
  }
  if (ans$type==3){
    low <- ans$interval$lower.limit
    up <- ans$interval$upper.limit
    cat("\nConfidence interval at the end of the trial: \n", "\nConfidence level: ", ans$level, "\nLast Z value: ", ans$fzvalue, "\n", 100*ans$level, "% confidence interval: (", low, ",", up, ")\n") 
  }
  if (ans$type==4){
    cat("\nAdjusted p-value: \n", "\nOrdering method: ", ans$p.ordering, "\nLook: ", ans$analysis.time, "\nZ value observed at that time: ", ans$fzvalue, "\n", "P-value: ", ans$p.value, "\n") 
  }
}

#' @export
"summary.ldPower" <- function (object, ...) 
{
  z <- object
  if (!inherits(z, "ldPower")) 
    stop("'object' must inherit from class \"ldPower\"")
  ans <- list()
  ans$type <- z$type
  ans$n <- length(z$time)
  if ((ans$type==1)|(ans$type==2)){
    ans$power <- z$power
    ans$drift <- z$drift
    if (identical(z$time,z$time2)){           
      b <- matrix(NA, ans$n, 5)
      b[,1:5] <- c(z$time, z$lower.probs, z$upper.probs, z$exit.probs, z$cum.exit)
      colnames(b) <- c("Time", "Lower probs", "Upper probs", "Exit pr.", "Cum exit pr.")  
      ans$bounds1 <- b
    }
    else{           
      b <- matrix(NA, ans$n, 6)
      b[,1:6] <- c(z$time, z$time2, z$lower.probs, z$upper.probs, z$exit.probs, z$cum.exit)
      colnames(b) <- c("Time", "Time 2", "Lower probs", "Upper probs", "Exit pr.", "Cum exit pr.")
      ans$bounds1 <- b
    }   
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
  class(ans) <- "summary.ldPower"
  return(ans)
}

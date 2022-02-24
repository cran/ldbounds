"adj.p" <- function(method,pvaltime,zval,bounds,t,up.bound){
    if (!method%in%c("SW","LR")){
      stop("Possible p-value orderings are stagewise (SW) and likelihood ratio (LR).")
    }
    if(!missing(bounds) && inherits(bounds,"ldBounds")){
      t <- bounds$time
      up.bound <- bounds$upper.bounds
    }
if (is.null(pvaltime)){
      stop("P-value time must correspond to one of the analysis times.")
    }
    if (!is.null(pvaltime)){
      if (pvaltime>length(up.bound)){
        stop("P-value time must correspond to one of the analysis times.")
      }
    }    
    if (method=="SW"){
      p.drift <- ldPower(zb=c(up.bound[1:(pvaltime-1)],zval),za=rep(-10,3),t=t[1:pvaltime],drift=0)
      p.value <- summary(p.drift)$bounds1[,'Cum exit pr.'][pvaltime]
    }
    else{
      lr.exit <- rep(0,length(up.bound))
      maxval1 <- max(up.bound[1],zval)
      lr1 <- ldPower(zb=maxval1,za=-10,t=t[1],drift=0)
      lr.exit[1] <- lr1$exit[1]
      for (j in 1:(length(up.bound)-1)){
        maxval <- max(up.bound[j+1],zval)
        lr <- ldPower(zb=c(up.bound[1:j],maxval),za=rep(-10,j+1),t=t[1:(j+1)],drift=0)
        lr.exit[j+1] <- lr$exit[j+1]
      }
      p.value <- sum(lr.exit)
    }
    return(p.value)
  }

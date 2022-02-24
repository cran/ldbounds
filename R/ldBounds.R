#' @export
"ldBounds" <- function(t,t2,iuse=1,asf=NULL,alpha=0.05,phi=rep(1,length(alpha)),
                     sides=2,ztrun=rep(Inf,length(alpha))){
  tol <- sqrt(.Machine$double.eps)
  if (!is.numeric(t)){
    stop("'t' must be a vector of analysis times or the number of analysis times")
  }
  if(length(t)==1){
    if(abs(t - round(t)) < tol & t > 1) t <- 1:t/t
    else if(t>1 ) stop("t must be an integer or in (0,1]")
  }
  if(missing(t2)) t2 <- t
  if (length(t) != length(t2)){
    stop("Original and second time scales must be vectors of the same length.")
  }
  if ({min(t) < tol}|{max(t) > 1+tol}|{min(t2) < tol}){
    stop("Analysis times must be in (0,1].  Second time scale values must be positive.")
  }
  t3 <- t2
  t2 <- t2/max(t2)
  if (any(diff(t) < tol)|any(diff(t2) < tol)){
    stop("Analysis times must be ordered from smallest to largest.")
  }
  if (any(alpha < tol)|{sum(alpha) > 1+tol}){
    stop("Each component of alpha must be positive and their sum cannot exceed 1.")
  }
  if (!sides%in%c(1,2)){
    stop("Sides must be 1 or 2.")
  }
  if(sides==1){
    if(iuse==5) {
      if(is.function(asf)) asf <- list(asf)    
      else if(is.list(asf)) {
        if(!is.function(asf[1]))
          stop("If iuse==5, asf must be a function or list of functions.")
      }
      alpha <- asf[[1]](1)
    }
    type <- 1
  }
  else {
    sym <- function(x)  length(x)==1 || x[1]==x[2]  
    type <- if(sym(iuse)&&(sym(phi)|!iuse[1]%in%3:4)&&
               (length(asf)==1|iuse[1]!=5)&&(sym(alpha)|iuse[1]==5)) 2
    else 3
    if(length(alpha)==1 & type==3)
      warning("Asymmetric boundary with alpha of length 1.")
    if(type==2){ 
      if(iuse[1]==5) {
        if(is.function(asf)) asf <- list(asf) 
        else if(is.list(asf)) {
          if(!is.function(asf[1]))
            stop("If iuse==5, asf must be a function or list of functions.")
        }
        alpha <- asf[[1]](1)
      }
    }
    else {
      iuse <- rep(iuse,length=2)
      phi <- rep(phi,length=2)
      ztrun <- rep(ztrun,length=2)
      asfTmp <- list(NULL,NULL)
      alphaTmp <- c(NA,NA)
      for(i in 1:2) {
        if(iuse[i]==5){
          if(is.list(asf) && is.function(asf[[i]])) 
            asfTmp[[i]] <- asf[[i]]
          else if(is.function(asf))
            asfTmp[[i]] <- asf
          else stop("asf must be a function or list of functions.")
          alphaTmp[i] <- asfTmp[[i]](1)
          if(length(alpha)==2) warning(paste("alpha for", c("lower","upper")[i],
                       "boundary ignored."))
        }
        else{
          alphaTmp[i] <- if(length(alpha)==1) alpha/2 else alpha[i]
          if(iuse[i] == 3 & phi[i]<=0) stop("For power family (iuse=3), phi must be positive.")
          else if(iuse[i] == 4 & phi[i]==0)
            stop("For Hwang-Shih-DeCani family (iuse=4), phi cannot be 0.")
        }}
      alpha <- alphaTmp
      asf <- asfTmp
    }
  }
  if (type<=2){
    ld <- landem(t,t2,sides,iuse[1],asf[[1]],alpha[1],phi[1],ztrun[1])
    ubnd <- ld$upper.bounds
    lbnd <- ld$lower.bounds
    epr <- ld$exit.pr
    dpr <- ld$diff.pr
    spend <- ld$spend
    if (type==2) alpha <- c(alpha/2,alpha/2)
  }
  else{
    ld1 <- landem(t,t2,1,iuse[1],asf[[1]],alpha[1],phi[1],ztrun[1])
    ld2 <- landem(t,t2,1,iuse[2],asf[[2]],alpha[2],phi[2],ztrun[2])
    lbnd <- -ld1$upper.bounds
    ubnd <- ld2$upper.bounds
    epr <- ld1$exit.pr+ld2$exit.pr
    dpr <- ld1$diff.pr+ld2$diff.pr
    spend <- c(ld1$spend,ld2$spend)
  }
  nom.alpha <- 1-pnorm(ubnd)+pnorm(lbnd)
  if (type==3) nom.alpha <- rep(NA,length(nom.alpha))
  ans <- list(bounds.type=type,spending.type=spend,time=t,time2=t3,alpha=alpha,
              overall.alpha=sum(alpha),lower.bounds=lbnd,upper.bounds=ubnd,exit.pr=epr,
              diff.pr=dpr,nom.alpha=nom.alpha)
  class(ans) <- "ldBounds"
  return(ans)
}

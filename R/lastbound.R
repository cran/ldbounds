#' @export
"lastbound" <- function(t,t2,alpha=0.05,sides=2,za=NULL,zb){
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
  if ({is.null(za)}&{!is.null(zb)})
    za <- -zb
  if(length(t)!=length(zb)+1) stop("The length of zb should be one less than the number of analysis times.")
  if(length(t)!=length(za)+1) stop("The length of za should be one less than the number of analysis times.")
  if({sides==1}&{length(alpha)==2}){
    stop("Specify only one alpha for one-sided bounds.")
  }
  if ({length(alpha)==1}|{{length(alpha)==2}&{alpha[1]==alpha[2]}&{all(za==-zb)}}){
      if ({length(alpha)==1}&{sides==2}){
        alph <- alpha/2
      }
      else{
        alph <- alpha
      }
      spend <- "User defined to obtain final boundary value."
      drift.pe <- ldPower(t[-length(t)],za=rep(-10,length(zb)),zb=zb,drift=0)
      pe.asf <- c(drift.pe$cum.exit,alph[1])
      ld <- landem(t,t2,1,iuse=NULL,alpha=alph[1],ztrun=10,pe=pe.asf)
      ubnd <- c(zb,ld$upper.bounds[length(t)])
      if ({length(alpha)==1}&{sides==1}){
        type <- 4
        lbnd <- rep(-8,length(ubnd))
      }
      if ({length(alpha)==2}|{{length(alpha)==1}&{sides==2}}){
        type <- 5
        lbnd <- -1*ubnd
      }
      drift.for.probs <- ldPower(za=lbnd,zb=ubnd,t=t2,drift=0)
      dpr <- drift.for.probs$upper.probs+drift.for.probs$lower.probs
      epr <- cumsum(dpr)
    }
    else{
      type <- 6
      spend <- c("User defined to obtain final boundary value.","User defined to obtain final boundary value.")
      drift.pe <- ldPower(t[-length(t)],za=rep(-10,length(za)),zb=-za,drift=0)
      pe.asf1 <- c(drift.pe$cum.exit,alpha[1])
      drift.pe <- ldPower(t[-length(t)],za=rep(-10,length(zb)),zb=zb,drift=0)
      pe.asf2 <- c(drift.pe$cum.exit,alpha[2])
      ld1 <- landem(t,t2,1,iuse=NULL,alpha=alpha[1],ztrun=10,pe=pe.asf1)
      ld2 <- landem(t,t2,1,iuse=NULL,alpha=alpha[2],ztrun=10,pe=pe.asf2)
      ubnd <- c(zb,ld2$upper.bounds[length(t)])
      lbnd <- c(za,-ld1$upper.bounds[length(t)])
      drift.for.probs <- ldPower(za=lbnd,zb=ubnd,t=t2,drift=0)
      dpr <- drift.for.probs$upper.probs+drift.for.probs$lower.probs
      epr <- cumsum(dpr)
    }
    nom.alpha <- 1-pnorm(ubnd)+pnorm(lbnd)
    if (type==6) nom.alpha <- rep(NA,length(nom.alpha))
    ans <- list(bounds.type=type,spending.type=spend,time=t,time2=t3,alpha=alpha,overall.alpha=sum(alpha),lower.bounds=lbnd,upper.bounds=ubnd,exit.pr=epr,diff.pr=dpr,nom.alpha=nom.alpha)
    class(ans) <- "ldBounds"
    return(ans)
}

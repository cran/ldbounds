#' @export
"commonbounds" <- function(looks=NULL,t=(1:looks)/looks,t2=t,iuse="OF",alpha=0.05,sides=2){
    if ({!is.null(looks)}&{!is.numeric(looks)}){
      stop("'looks' must be an integer.")
    }
    if (sum(abs(t-(1:length(t))/length(t))<0.0000001)<length(t)){
      warning("Time points are not equally spaced.")
    }
    if (length(t) != length(t2)){
      stop("Original and second time scales must be vectors of the same length.")
    }
    if ({min(t) < 0.0000001}|{max(t) > 1.0000001}|{min(t2) < 0.0000001}){
      stop("Analysis times must be in (0,1].  Second time scale values must be positive.")
    }
    t3 <- t2
    t2 <- t2/max(t2)
    if (any({diff(t) < 0.0000001})|any(diff(t2) < 0.0000001)){
      stop("Analysis times must be ordered from smallest to largest.")
    }
    if (sum(!iuse%in%c("PK","OF"))>0){
      stop("Boundary type (iuse) must be \"PK\" or \"OF\".")
    }
    if (any(alpha < 0.0000001)|{sum(alpha) > 1.0000001}){
      stop("Each component of alpha must be positive and their sum cannot exceed 1.")
    }
    if (length(iuse) != length(alpha)){
      stop("For two-sided bounds, the lengths of the iuse and alpha vectors must both be 2.")
    }
    if (!sides%in%c(1,2)){
      stop("Sides must be 1 or 2.")
    }
    if (is.null(looks)){
      looks <- length(t)
    }
    if ({length(alpha)==1}|{{length(alpha)==2}&{alpha[1]==alpha[2]}&{iuse[1]==iuse[2]}}){
      if ({length(alpha)==1}&{sides==2}){
        alph <- alpha/2
      }
      else{
        alph <- alpha
      }
      if (iuse[1]=="PK"){
        root <- uniroot(search.glan.pocock,c(1.5,qnorm(1-alph)+0.3+0.05*looks),k=looks,alpha=alph)$root
        ubnd <- rep(root,looks)
        spend <- "Pocock"
      }
      if (iuse[1]=="OF"){
        root <- uniroot(search.glan.obrien,c(1.5,qnorm(1-alph)+0.05*looks),k=looks,alpha=alph)$root
        ubnd <- root/sqrt((1:looks)/looks)
        spend <- "O'Brien-Fleming"
      }
      if ({length(alpha)==1}&{sides==1}){
        type <- 4
        lbnd <- rep(-8,length(ubnd))
      }
      if ({length(alpha)==2}|{{length(alpha)==1}&{sides==2}}){
        type <- 5
        lbnd <- -1*ubnd
      }
      drift.for.probs <- ldPower(za=lbnd,zb=ubnd,t=t2,drift=0)
      dpr <- drift.for.probs$upper.probs
      epr <- cumsum(dpr)
    }
    else{
      type <- 6
      spend <- c("","")
      if (iuse[1]=="PK"){
        root <- uniroot(search.glan.pocock,c(1.5,qnorm(1-alpha[1])+0.3+0.05*looks),k=looks,alpha=alpha[1])$root
        lbnd <- -1*rep(root,looks)
        spend[1] <- "Pocock"
      }
      if (iuse[1]=="OF"){
        root <- uniroot(search.glan.obrien,c(1.5,qnorm(1-alpha[1])+0.05*looks),k=looks,alpha=alpha[1])$root
        lbnd <- -1*root/sqrt((1:looks)/looks)
        spend[1] <- "O'Brien-Fleming"
      }
      if (iuse[2]=="PK"){
        root <- uniroot(search.glan.pocock,c(1.5,qnorm(1-alpha[2])+0.3+0.05*looks),k=looks,alpha=alpha[2])$root
        ubnd <- rep(root,looks)
        spend[2] <- "Pocock"
      }
      if (iuse[2]=="OF"){
        root <- uniroot(search.glan.obrien,c(1.5,qnorm(1-alpha[2])+0.05*looks),k=looks,alpha=alpha[2])$root
        ubnd <- root/sqrt((1:looks)/looks)
        spend[2] <- "O'Brien-Fleming"
      }
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

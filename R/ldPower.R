#' @export
"ldPower" <- function(t,za=NULL,zb=NULL,t2=t,pow=NULL,drift=NULL,conf=NULL,method=NULL,
                    pvaltime=NULL,zval=zb[length(zb)]){
  tol <- sqrt(.Machine$double.eps)
    if (inherits(t, "ldBounds")){
      za <- t$lower.bounds
      zb <- t$upper.bounds
      t2 <- t$time2
      t <- t$time
    }
    else {
      if(length(t)==1){
        if(abs(t - round(t)) < tol & t > 1) t <- 1:t/t
        else if(t>1) stop("t must be an integer or in (0,1]")
      }
      if(missing(t2)) t2 <- t
      else if (length(t) != length(t2)){
        stop("Original and second time scales must be vectors of the same length.")
      }
      if ({min(t) < tol}|{max(t) > 1+tol}|{min(t2) < tol}){
        stop("Analysis times must be in (0,1].  Second time scale values must be positive.")
      }
      if ({min(t) <= 0}|{max(t) > 1}|{min(t2) <= 0}){
        stop("Analysis times must be in (0,1].  Second time scale values must be positive.")
      }}
    if (any({diff(t)<= 0}|{diff(t2) <= 0})){
      stop("Analysis times must be strictly increasing.")
    }
  if ({is.null(za)}&{!is.null(zb)})
    za <- -zb
  if(length(t)!=length(zb)) stop("t and zb have different lengths.")
  if(length(t)!=length(za)) stop("t and za have different lengths.")
  t3 <- t2
  t2 <- t2/max(t2)
  if ((!is.null(pow))+(!is.null(drift))+(!is.null(conf))+(!is.null(method))>1){
    stop("Only one of power, drift, confidence level, or p-value ordering can be given.")
  }
  else if (is.null(pow)&is.null(drift)&is.null(conf)&is.null(method)){
    drift=0
    }
    drift1 <- NULL
    za[za==-Inf] <- -8
    zb[zb==Inf] <- 8
    if (!is.null(pow)){
      if ({pow <= 0}|{pow > 1}){
        stop("Power must be in (0,1].")
      }
      type <- 1
      drift1 <- adrift(t2,za,zb,pow)
    }
    if (!is.null(drift)){
      type <- 2
      drift1 <- drift
    }
    if (!is.null(drift1)){
      gl <- glan(t2,za,zb,drift1)
      if (!is.null(drift)) pow <- gl$pr
      za[za<7.9995*(-1)] <- -Inf
      zb[zb>7.9995] <- Inf
      gl$qneg[gl$qneg<tol] <- 0
      gl$qpos[gl$qpos<tol] <- 0
      ans <- list(type=type,time=t,time2=t3,lower.bounds=za,upper.bounds=zb,power=pow,
                  drift=drift1,lower.probs=gl$qneg,upper.probs=gl$qpos,
                  exit.probs=gl$qneg+gl$qpos,cum.exit=cumsum(gl$qneg+gl$qpos))
    }
    if (!is.null(conf)){
      if (zval < 0){
        stop("Confidence interval is only for nonnegative final Z value.")
      }
      conf.limit <- ci(conf,zval,t2,za,zb)
      za[za<7.9995*(-1)] <- -Inf
      zb[zb>7.9995] <- Inf
      ans <- list(type=3,time=t,time2=t3,lower.bounds=za,upper.bounds=zb,
                  conf.level=conf,final.zvalue=zval,conf.interval=conf.limit)
    }
    if (!is.null(method)){
      if (zval < 0){
        stop("P-value is only for nonnegative Z value.")
      }
      p.value <- adj.p(method,pvaltime,zval,t=t2,up.bound=zb)
      za[za<7.9995*(-1)] <- -Inf
      zb[zb>7.9995] <- Inf
      ans <- list(type=4,time=t,time2=t3,lower.bounds=za,upper.bounds=zb,
                  conf.level=conf,analysis.time=pvaltime,final.zvalue=zval,p.ordering=method,p.value=p.value)
    }
    class(ans) <- "ldPower"
    return(ans)
  }

#' @export
"plot.ldPower" <- function(x, scale = "z", main = NULL, xlab = NULL, ylab = NULL,
           xlim, ylim, las=1, pch=19, type="o",add=F, ...){
    if (!((inherits(x, "ldBounds"))|(inherits(x, "ldPower"))))
      stop("'x' must inherit from class \"ldBounds\" or \"ldPower\"")
    if (!scale%in%c("z","b"))
      stop("Scale must be either \"z\" (z-value) or \"b\" (b-value)")
    if (is.null(main))
      main <- "Sequential boundaries using the Lan-DeMets method"
    if (is.null(xlab))
      xlab <- "Information Fraction"
    if (is.null(ylab)){
      if (scale=="z"){
        ylab <- "Z"
      }
      else{
        ylab <- "B"
      }
    }
    z <- c(0,x$time)
    r <- rep(0,length(z))
    if(missing(xlim)) xlim <- c(0,z[length(z)])
    if ({inherits(x, "ldBounds")}&&{x$bounds.type==1}){  ### TDC added extra "&" 
      u <- c(NA,x$upper.bounds)
      if (scale=="b"){
        u <- u*sqrt(z)
      }
      if(missing(ylim)) ylim <- c(0,max(u,na.rm=T))
      if(add) lines(z,u, pch=pch, type=type,...)
      else plot(z,u, main = main, xlab = xlab, ylab = ylab, xlim=xlim, ylim=ylim,
                las=las, pch=pch, type=type,...)
      points(z,r, ...)
      lines(z,r,...)
    }                                 
    else{
      u <- c(NA,x$upper.bounds)
      l <- c(NA,x$lower.bounds)
      if (scale=="b"){
        u <- u*sqrt(z)
        l <- l*sqrt(z)
      }
      if(missing(ylim)) ylim <- c(min(l,na.rm=T),max(u,na.rm=T))
      if(add) lines(z,u, pch=pch, type=type,...)
      else plot(z,u, main = main, xlab = xlab, ylab = ylab, xlim=xlim, ylim=ylim, las=las, pch=pch,
                type=type,...)
      points(z,l,pch=19, ...)
      lines(z,l,...)
      points(z,r, ...)
      lines(z,r,...)
    }
  }

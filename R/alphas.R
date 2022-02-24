"alphas" <-
function(iuse,asf,alpha,phi,side,t,pe=NULL){
    tol <- 10^(-13)
    if (!is.null(pe)){
      pe <- pe
      spend <- ""
    }
    else if (iuse==1){
        pe <- 2*(1-pnorm(qnorm(1-(alpha/side)/2)/sqrt(t)))
        spend <- "O'Brien-Fleming"
    }
    else if (iuse==2){
        pe <- (alpha/side)*log(1+(exp(1)-1)*t)
        spend <- "Pocock"
    }
    else if (iuse==3){
        pe <- (alpha/side)*t^phi
        spend <- if(phi==1) "Power Family: alpha * t"
        else paste("Power Family: alpha * t^", round(phi,2),sep="")
    }
    else if (iuse==4){
        pe <- (alpha/side)*(1-exp(-phi*t))/(1-exp(-phi))
        spend <- "Hwang-Shih-DeCani Family"
    }
    else if (iuse==5){
     if(missing(alpha)) alpha <- asf(1)
     if(any(diff(asf(t))<=0.0000001))
       stop("Alpha Spending function must an increasing function.")
     if(asf(1)>1 ) stop("Alpha Spending function must be less than or equal to 1.")
     spend <- "User-specified spending function"
     pe <- (1/side)*asf(t)
    }
    else stop("Must choose 1, 2, 3, 4, or 5 as spending function.")
    pe <- side*pe
    pd <- diff(c(0,pe))
    if (sum(as.integer({pd<0.0000001*(-1)}|{pd>1.0000001})) >= 1){
        warning("Spending function error")
        pd <- min(1,pd)
        pd <- max(0,pd)
    }
    for (j in 1:length(pd)){
        if (pd[j] < tol){
            warning("Type I error spent too small for analysis #",j,"\n",
                    "Zero used as approximation for ",pd[j])
            pd[j] <- 0
        }
    }
    ans <- list(pe=pe,pd=pd,spend=spend)
    return(ans)
}


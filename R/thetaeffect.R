"thetaeffect" <- function(outcome.type,par.c,par.t,N,sigma){
    if (outcome.type=="bin"){
        pbar <- (par.c+par.t)/2
        theta <- (par.c-par.t)/sqrt(2*pbar*(1-pbar)/(N/2))
    }
    else if (outcome.type=="mean"){
        theta <- (par.c-par.t)*sqrt(N)/2/sigma
    }
    else{
        theta <- sqrt(N/4)*log(par.c/par.t)
    }
    return(theta)
}

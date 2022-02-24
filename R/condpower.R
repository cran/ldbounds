#' @export
"condpower" <- function(z.crit,z.val,accr,outcome.type,par.c,par.t,N,sigma=NULL){
    if (z.crit<=0){
        stop("The final critical value must be positive.")
    }
    accr <- round(accr)
    if (accr<0){
        stop("The number accrued cannot be negative.")
    }
    if (!outcome.type%in%c("bin","mean","surv")){
        stop("The outcome type must be binary ('bin'), means ('mean'), or survival ('surv').")
    }
    if (outcome.type=="bin"){
        if ({par.c<0}|{par.c>1}|{par.t<0}|{par.t>1}){
            stop("Binary probabilities must be in [0,1].")
        }
    }
    if (outcome.type=="mean"){
        if (is.null(sigma)){
            stop("Must specify sigma for continuous outcome.")
        }
        if (is.null(par.t)){
            par.t <- 0
        }
    }
    if (outcome.type=="surv"){
        if ({par.c<=0}|{par.t<=0}|{par.c<par.t}){
            stop("Hazards must be positive and control hazard must be less than treatment hazard.")
        }
        if (is.null(par.t)){
            par.t <- 1
        }
    }
    I.frac <- accr/N
    B.val <- z.val*sqrt(I.frac)
    theta <- thetaeffect(outcome.type,par.c,par.t,N,sigma)
    cond.pow <- 1-pnorm((z.crit-B.val-(1-I.frac)*theta)/sqrt(1-I.frac))
    return(cond.pow)
}

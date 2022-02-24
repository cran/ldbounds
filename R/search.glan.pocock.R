"search.glan.pocock" <- function(k,c,alpha){
    return(glan((1:k)/k,rep(-8,k),rep(c,k),0)$pr-alpha)
  }

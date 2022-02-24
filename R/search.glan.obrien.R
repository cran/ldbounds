"search.glan.obrien" <- function(k,c,alpha){
    return(glan((1:k)/k,rep(-8,k),c/sqrt((1:k)/k),0)$pr-alpha)
  }

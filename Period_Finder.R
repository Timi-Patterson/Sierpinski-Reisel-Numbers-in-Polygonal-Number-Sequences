modLists <- function(list, modulus){
  solnList <- c()
  for(x in list){
    solnList <- c(solnList, mod(x, modulus))
  }
  return(solnList)
}




periodFinder <- function(list, modulus=-1){
  l <- list
  m <- modulus
  if (m != -1){
    check <- c(modLists(l,m))
  }
  else{
    check <- c(list)
  }
  n <- 1
  runthroughloop <- 0
  upperlimit <- 70000
  
  checkIndex <- c(check[1])
  nextcheck <- c(check[2])
  
  while(n<=upperlimit){
    if(identical(checkIndex,nextcheck) == TRUE){
      
      return(paste0("n=",n-1))
      
    }
    
    if(identical(checkIndex,nextcheck) == FALSE){
      x <- 0
      checkIndex <- c()
      nextcheck <- c()
      
      
      
      while(x <= n){
        checkIndex <- c(checkIndex, check[x])
        x <- x+1
      }
      
      while(x>n && x <= 2*n){
        nextcheck <- c(nextcheck, check[x])
        x <- x+1
      }
      
      if(n==upperlimit){
        return("No match")
      }else {
        n <- n+1
      }
    }
  } 
  
}

SierRiesFinder <- function(list_of_mods, list_of_bags){
  bigmod <- prod(list_of_mods)
  Lcomp <- c()
  for (x in 0:bigmod){
    Lcomp <- c(Lcomp, x)
  }
  for (x in 1:(length(list_of_mods))){
    Lcomp <- Lcomp[which((Lcomp %% list_of_mods[x]) %in% list_of_bags[[x]])]
    print(Lcomp)
  }
  return(Lcomp)
}

mods <- c(5, 7, 13, 19, 73, 37)
bags <- list(c(1,5),c(0,2,4,6),c(3,4,8,9),c(8,9,10),c(12,13,59,60),c(16,20))
SierRiesFinder(mods, bags)


mods <- c(2,3,5)
bags <- list(c(0),c(0,1),c(0,2,3))
SierRiesFinder(mods, bags)

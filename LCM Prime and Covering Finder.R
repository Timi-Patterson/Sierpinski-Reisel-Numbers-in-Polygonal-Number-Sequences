
findPrimesAndCoverings <- function(lcmvalue){
Lv <- factorize(as.bigz(2)^lcmvalue-1)
print(Lv)
for (p in 1:length(Lv)){
  L <- c()
  for (x in seq.int(0,(lcmvalue*2 + 5))){
    L <- append(L, as.bigz(2^x, as.bigz(Lv[p])))
  }
  print(periodFinder((as.numeric(L))))
  
}
}
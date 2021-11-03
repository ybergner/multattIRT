
reshapeItemParams <- function(itemparams) {
  n <- nrow(itemparams)
  a.index <- seq(1,n-2,3)
  b1.index <- seq(1,n-2,3)
  b2.index <- seq(2,n-1,3)
  b3.index <- seq(3,n,3)
  item.comp <- array(NA,c(n/3,4))
  item.comp[,1] <- as.numeric(itemparams[a.index,1])
  item.comp[,2] <- as.numeric(itemparams[b1.index,2])
  item.comp[,3] <- as.numeric(itemparams[b2.index,2])
  item.comp[,4] <- as.numeric(itemparams[b3.index,2])
  return(item.comp)
}

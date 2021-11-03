
#' itemMaxAtts is used to obtain a vector of maximum attempts based on a target proportion correct
#'
#' @param U a response matrix in OAC format
#' @param targetProp proportion correct per item above which no more attempts will be counted (default 0.85)
#' @param hardMax an upper limit on attempts regardless of proportion (default 4)
#'
#' @return a vector of attempts of length = ncol(U)
#' @export
#'
itemMaxAtts <- function(U, targetProp=0.85, hardMax=4) {
  maxAtts <- c()
  for (i in 1:ncol(U)) {
    atts <- 1
    propCor <- length(which(U[,i] > 0 & U[,i] <= atts))/length(which(!is.na(U[,i])))
    while (propCor < targetProp & atts < hardMax) {
      atts <- atts + 1
      propCor <- length(which(U[,i] > 0 & U[,i] <= atts))/length(which(!is.na(U[,i])))
    }
    maxAtts <- c(maxAtts, atts)
  }
  return(maxAtts)
}

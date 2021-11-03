#' countGiveUps
#'
#' @param X, a response matrix of dimension nS x nI
#' @param numAtts, a vector of the max attempts allowed per item
#'
#' @return a list containing the counts of giveups and the counts of total wrong attempts (each vector of length nS)
#' @export
#'
countGiveUps <- function(X, numAtts) {
  giveups <- apply(X,1, FUN=function(row){sum(row < 0 & row > -numAtts, na.rm=T)})
  totAtts <- rowSums(abs(X), na.rm=T)
  totCorrect <- rowSums(X > 0, na.rm=T)
  totWrongAtts <- totAtts - totCorrect
  rate <- giveups/totWrongAtts
  rate[!is.finite(rate)] <- 0 # division by 0 occurs sometimes when there are no Wrong attempts
  return(list(giveups=giveups,totWrongAtts=totWrongAtts, totAtts=totAtts,rate=rate))
}

#' maxOutOAC
#'
#' @param X
#' @param numAtts
#'
#' @return a matrix with OAC values less than or equal to max attempts
#' @export
#'
maxOutOAC <- function(X,numAtts) {
  for (i in 1:ncol(X)) {
    X[which(abs(X[,i]) > numAtts[i]),i] <- -numAtts[i]
  }
  return(X)
}

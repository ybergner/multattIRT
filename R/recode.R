### reCode: to go between OAC <--> MultAtt representations

#' reCode SM->GRM
#'
#' This function and \code{\link{reCodeGRM2SM}} convert data representations between OAC (observed attempt counts)
#' and graded polytomous scores. This mapping is NOT one-to-one.
#'
#' @param U a response matrix in OAC format, see \code{\link{responses}}
#' @param itemAllowedAttempts a vector of length numItems, where each element is the maximum allowed attempts
#' for converting to an graded polytomous representation.
#'
#' @return Output is a response matrix in ordered form.
#' @export
#'
#' @examples
#' tmp <- responses[1:10,1:5]
#' reCodeSM2GRM(tmp, rep(3,ncol(tmp)))
#'
reCodeSM2GRM <- function(U,itemAllowedAttempts) {
  for (i in 1:ncol(U)) {
    U[which(U[,i] > itemAllowedAttempts[i] | U[,i] < 0),i] <- (itemAllowedAttempts[i] + 1)
    U[,i] <- itemAllowedAttempts[i] - U[,i] + 1
  }
  return(U)
}

#' reCode GRM->SM
#'
#' This function and \code{\link{reCodeSM2GRM}} convert data representations between from
#' OAC (observed attempt counts) and graded polytomous responses. This mapping is NOT one-to-one.
#'
#' @param U a response matrix in graded polytomous format
#' @param itemAllowedAttempts a vector of length numItems, where each element is the maximum allowed attempts.
#'
#' @return Output is a response matrix in OAC form, see \code{\link{responses}}
#' @export
#'
#' @examples
#' tmp <- responses[1:10,1:5]
#' tmp2 <- reCodeSM2GRM(tmp, rep(3,ncol(tmp)))
#' tmp3 <- reCodeGRM2SM(tmp2, rep(3,ncol(tmp)))

reCodeGRM2SM <- function(U,itemAllowedAttempts) {
  for (i in 1:ncol(U)) {
    U[which(U[,i] == 0),i] <- -itemAllowedAttempts[i]
    U[which(U[,i] > 0),i] <- itemAllowedAttempts[i] - U[which(U[,i] > 0),i] + 1
  }
  return(U)
}


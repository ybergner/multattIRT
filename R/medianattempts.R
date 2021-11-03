#' Median Attempts
#'
#' This function is useful in response matrices with missing data (NAs) and also
#' possible gaming behaviors, such as students with very large numbers of observed attempts
#'
#' @param U a response matrix containing observed attempts to correct. U is
#'   integer-valued, and $U_ij$ = n, where n <0 if the nth attempt was (still)
#'   incorrect.
#' @param maxAttempts an integer denoting the cutoff M, such that attempt values
#'   greater than M are ignored in finding the median. The only reason to use
#'   this argument is to remediate abuse/gaming of the system.
#' @param useAbs Boolean, default is \code{FALSE}, in which case only correct
#'   attempts are included in finding the median. If \code{TRUE} then incorrect
#'   attempts are also counted after taking absolute values.
#'
#' @return a vector of length \code{nrow(U)}
#' @export
#'
#' @examples
#'
#' medianattempts(responses)
medianattempts <- function(U, maxAttempts=99, useAbs = F) {
  # # # medianattempts: compute the median attempts to correct for each item
  # # # takes a response matrix U
  # # # returns a vector of length ncol(U)
  medianAttempt <- c()
  if (useAbs) {
    for (i in 1:ncol(U)) {
      medianAttempt[i] = median(abs(U[which(!is.na(U[,i]) & abs(U[,i]) < maxAttempts),i]))
    }
  } else {
    for (i in 1:ncol(U)) {
      medianAttempt[i] = median(U[which(!is.na(U[,i]) & U[,i] > 0 & U[,i] < maxAttempts),i])
    }
  }
  return(medianAttempt)
}

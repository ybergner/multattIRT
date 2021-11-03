## FUNCTION FOR RMSD
#' get.rmsd
#'
#' @param x
#' @param y
#'
#' @return root mean squared deviation
#' @export
#'
get.rmsd <- function(x,y){
  sqrt(mean((x-y)^2))
}

#' Logit function
#'
#' \code{logit} computes log(z/(1-z)), whereas
#' \code{\link{inv.logit}} return the inverse
#'
#' @param z a real value in the open interval (0,1), otherwise NaNs produced.
#'
#' @return Output is the value of the function log(z/(1-z))
#' @export
#'
#' @examples
#' z <- 5
#' logit(z)
#' inv.logit(logit(z))
logit <- function(z) {
  log(z/(1-z))
}

#' Inverse Logit, aka Logistic function
#'
#' \code{inv.logit} computes 1/(1+exp(-z)), the inverse of
#' \code{\link{logit}}
#'
#' @param z a real valued number.
#'
#' @return Output is the value of the function 1/(1+exp(-z)), on the interval [0,1]
#' @export
#'
#' @examples
#' z <- 5
#' inv.logit(z)
#' logit(inv.logit(z))
inv.logit <- function(z){
  1/(1+exp(-z))
}

logitRasch <- function(sk, d) {
  inv.logit(sk-d)
}

logistic2pl <- function(sk, d, a) {
  inv.logit(a*(sk-d))
}

logistic2pl.vec <- function(sk, d, a) {
  ### uses vectors and returns matrix
  smat = matrix(rep(sk,length(d)),nrow=length(sk))
  dmat = matrix(rep(d,length(sk)),nrow=length(sk),byrow=T)
  amat = matrix(rep(a,length(sk)),nrow=length(sk),byrow=T)
  inv.logit(amat * (smat - dmat))
}


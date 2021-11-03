inv.logit <- function(z){
  1/(1+exp(-z))
}

logitRasch <- function(sk, d) {
  inv.logit(sk-d)
}

logistic2pl <- function(sk, d, a) {
  inv.logit(a*(sk-d))
}

reshape.params <- function(pparams, iparams, numAtts) {
  slopes <- unlist(iparams$a)
  thresholds <- unlist(iparams$b)
  I <- cbind(-thresholds*slopes, slopes)
  thetas <- pparams$theta
  Theta = cbind(rep(1,length(thetas)),thetas)
  return(list(I=I,Theta=Theta))
}

# assume a sequential format with 2pjk parameter degrees of freedom
manLogLik2 <- function(X, numAtts, estimObj, usex) {

  Xplus <- makeXplus(X, numAtts); cat("+")
  Xnaught <- makeXnaught(Xplus, numAtts); cat("o")
  if (usex == "Xnaught") {
    X <- Xnaught
  } else if (usex == "Xplus") {
    X <- Xplus
  } else {
    stop("Value of usex must be in c(Xplus, Xnaught)")
  }

  thetas <- estimObj$personPars[,"F1"]
  slopes <- estimObj$itemPars[,"a"]
  thresholds <- estimObj$itemPars[,"b"]

  # we are going to roll up the parameters so that we can use matrix math and avoid loops
  I <- cbind(-thresholds*slopes, slopes)
  Theta = cbind(rep(1,length(thetas)),thetas)

  P = sigmoid(Theta %*% t(I))
  LL = sum(log(P)*X + log(1-P)*(1-X), na.rm=T)
  return(LL)
}


manLogLik <- function(estimObj) {
  # use if X is already in sequential form
  X <- estimObj$fitObj@Data$data
  thetas <- estimObj$personPars[,"F1"]
  slopes <- estimObj$itemPars[,"a"]
  thresholds <- estimObj$itemPars[,"b"]

  # we are going to roll up the parameters so that we can use matrix math and avoid loops
  I <- cbind(-thresholds*slopes, slopes)
  Theta = cbind(rep(1,length(thetas)),thetas)

  P = sigmoid(Theta %*% t(I))
  LL = sum(log(P)*X + log(1-P)*(1-X), na.rm=T)
  return(LL)
}

# assume a sequential format with 2pjk parameter degrees of freedom
manLogLik3 <- function(estimObj, numAtts, usex) {

  Xplus <- estimObj$fitObj@Data$data
  if (usex == "Xnaught") {
    Xnaught <- makeXnaught(Xplus, numAtts)
    X <- Xnaught
  } else {
    X <- Xplus
  }

  thetas <- estimObj$personPars[,"F1"]
  slopes <- estimObj$itemPars[,"a"]
  thresholds <- estimObj$itemPars[,"b"]

  # we are going to roll up the parameters so that we can use matrix math and avoid loops
  I <- cbind(-thresholds*slopes, slopes)
  Theta = cbind(rep(1,length(thetas)),thetas)

  P = sigmoid(Theta %*% t(I))
  LL = sum(log(P)*X + log(1-P)*(1-X), na.rm=T)
  return(LL)
}

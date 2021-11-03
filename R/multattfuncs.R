# multattfuncs.r
#
# Main functions for simulating data from and estimating parameters
# for a set of models suitable for multiple attempt data
#
# require(mirt)

#' Simulate multiple attempt data
#'
#' \code{multatt.sim} returns the simulated values for person parameters,
#' item parameters, and a polytomous response matrix. The response matrix will
#' either follow the "graded" response convention if the generating model is GRM
#' or GPCM or the "attempts" count convention, if a sequential model is used.
#'
#'
#' @param model One of "GRM","GPCM","SM.Rasch","SM.2pj","SM.2pjx"
#' @param nS number of examinees
#' @param nI number of items
#' @param numAtts a vector of length nI containing allowed attempt numbers for each item
#' @param randomSeeds Two values, one for input parameters and one for the generated responses
#' @param orderedCats Logical; applies to the sequential model
#' @param paramcontrol A list of distribution parameters for threshold and discrimination parameters
#'
#' @return Output is a list containing pparams (person parameters), iparams
#'   (item parameters), and X (response matrix) integer.
#' @export
#'
#' @examples
#' nS = 10
#' nI = 5
#' numAtts <- c(2,2,3,3,3)
#' randomSeeds <- c(2015,2016)
#' simGRM <- multatt.sim(model="GRM", nS, nI, numAtts, randomSeeds)
#' simSMR <- multatt.sim(model="SM.Rasch", nS, nI, numAtts, randomSeeds)
#'
multatt.sim <- function(model, nS, nI, numAtts, randomSeeds,
                        orderedCats = FALSE,
                        pParamType = "nogiveup",
                        paramcontrol = list(mu.theta=0,sd.theta=1,
                                            multimu=c(0,-3), Sigma=matrix(c(1,-0.45,-0.45,1),nrow=2),
                                            mu.a=0,sd.mu.a=0.5,sd.a=0.2,
                                            mu.b=0,sd.mu.b=1,sd.b=0.3)) {

  #   nS is number of examinees
  #   nI is number of items
  #   numAtts is a vector on length nI, containing number of attempts allowed for each item
  #   randomSeeds for repeatability; optionally one or two if params and scores are simulated separately
  #   paramcontrol allows specification of distribution statistics as well as whether categories must
  #     be ordered or not in the case of the sequential models

  modellist <- c("GRM","GPCM","SM.Rasch","SM.2pj","SM.2pjx")
  if (!model %in% modellist) {
    stop("Unknown model detected! Model must be one of these: ", paste(modellist, collapse=", "))
    }
  if (length(numAtts) != nI) {stop("numAtts is not a vector of length nI!")}
  if (length(randomSeeds) != 2) {stop("randomSeeds must have 2 elements!")}

  X <- matrix(NA, nrow=nS, ncol=nI)

  # theta simulation is not model dependent
  # theta <- simulate.theta(nS,paramcontrol$mu.theta,paramcontrol$sd.theta,randomSeeds[1])
  #### placeholder values for missingness
  #### could include this in pparams
  omittitude <- rlnorm(nS, meanlog=-Inf, sdlog=0)    # placeholder for ranom tendency to omit items
  # giveupitude <- rbeta(nS, 1,Inf)    # placeholder for random tendency to stop if already incorrect
  pparams <- simulate.multidim(nS, type=pParamType, mu=paramcontrol$multimu, Sigma=paramcontrol$Sigma, seed=randomSeeds[1])
  pparams$omittitude <- omittitude

  # parameter simulation is model dependent
  if (model=="GRM") {
    nCats <- numAtts + 1 # additional category for "never correct"
    a <- simulate.a(nI,mu.a=paramcontrol$mu.a,sd.mu.a=paramcontrol$sd.mu.a,randomSeeds[1])
##    b <- simulate.b(nI,nCats,mu.b=paramcontrol$mu.b,sd.b=paramcontrol$sd.b,randomSeeds[1])
    b <- simulate.b(nI,nCats,mu.b=paramcontrol$mu.b,sd.mu.b=paramcontrol$sd.mu.b,
                    sd.b=paramcontrol$sd.b,seed=randomSeeds[1])
  } else if (model == "GPCM") {
		nCats <- numAtts + 1
    a <- simulate.a(nI,mu.a=paramcontrol$mu.a,sd.mu.a=paramcontrol$sd.mu.a,randomSeeds[1])
		b <- simulate.b.gpcm(nI,nCats,mu.b=paramcontrol$mu.b,sd.b=paramcontrol$sd.b,randomSeeds[1])
  } else if (length(grep("SM",model))) {
    # SM detected
    if (model == "SM.Rasch") {
      paramcontrol$sd.mu.a=0
      paramcontrol$sd.a=0
    } else if (model == "SM.2pj") {
      paramcontrol$sd.a=0
    }
    a <- simulate.a.sm(nI, numAtts,
                       mu.a = paramcontrol$mu.a,
                       sd.mu.a = paramcontrol$sd.mu.a,
                       sd.a = paramcontrol$sd.a,
                       randomSeeds[1])
    b <- simulate.b.sm(nI, numAtts, orderedCats = orderedCats,
                       mu.b = paramcontrol$mu.b,
                       sd.mu.b = paramcontrol$sd.mu.b,
                       sd.b = paramcontrol$sd.b,
                       randomSeeds[1])
  }
  iparams = list(a=a,b=b)
  # pparams = list(theta=theta)
  X <- generate.data(model, pparams, iparams, randomSeeds[2])
  return(list(pparams=pparams,iparams=iparams,X=X))
}

#' Generate response matrix given model and parameters
#'
#' @param model One of "GRM","GPCM", "SM"
#' @param pparams A list containing at least theta vector
#' @param iparams A list containing discrimination and threshold parameters (a,b)
#' @param scoreSeed A random seed
#'
#' @return A simulated response matrix
#' @keywords internal
#' @export
#'
generate.data <- function(model, pparams, iparams, scoreSeed) {
  a <- iparams$a
  b <- iparams$b
  theta <- pparams$theta
  giveupitude <- pparams$giveupitude
  omittitude <- pparams$omittitude
  nS <- length(theta)
  nI <- length(a)
  if (model=="GRM") {
    bottom.score <- rep(0,nI)
    X <- simulate.grm(a,b,theta,omittitude,giveupitude,bottom.score,scoreSeed)
  } else if (model == "GPCM") {
    bottom.score <- rep(0,nI)
		X <- simulate.gpcm(a,b,theta,omittitude,giveupitude,bottom.score,scoreSeed)
  } else if (length(grep("SM",model))) {
    # SM detected
   ####
    X <- simulate.sm(a, b, theta, omittitude, giveupitude, scoreSeed, debug=F)
    ####
  }
  return(X)
  }

#' Estimate parameters given multiple attempt data and model
#'
#' @param data A response matrix or data frame, with ncols = number of items
#'   (nI)
#' @param datatype Either "graded" or "attempts". Graded type means category
#'   labels run from 0...M where M is the highest category and corresponds to
#'   answering correct on the first attempt. A value of 0 corresponds to
#'   incorrectly exhausting all allowable attempts. In the "attempt"
#'   representation, values range from -M to M, excluding 0. See
#'   \code{\link{recodeGRM2SM}} and \code{\link{recodeSM2GRM}} for interchanging
#'   between these representations, although the mapping is not exactly
#'   one-to-one.
#' @param numAtts a vector of length nI containing max allowed attempt numbers
#'   for each item
#' @param model One of "GRM", "GPCM", "SM.Rasch", "SM.2pj", "SM.2pjx",
#'   "MIMA.Rasch", "MIMA.2pj", "MIMA.2pjx"
#'
#' @return A list containing the fitobject (fitObj), item parameters (itemPars),
#'   and person parameters (personPars)
#' @export
#'
#' @examples
#' nS = 500
#' nI = 9
#' numAtts <- c(2,2,2,3,3,3,4,4,4)
#' randomSeeds <- c(21,22)
#' simGRM <- multatt.sim(model="GRM", nS, nI, numAtts, randomSeeds)
#' simSMR <- multatt.sim(model="SM.Rasch", nS, nI, numAtts, randomSeeds)
#' #estGRMwithSMR <- multatt.estim(simGRM$X, datatype="graded", numAtts, model="SM.Rasch")
#' #estSMRwithGRM <- multatt.estim(simSMR$X, datatype="attempts", numAtts, model="GRM")

multatt.estim <- function(data, datatype, numAtts, model) {
  nI <- ncol(data)
  modellist <- c("GRM","GPCM",
                 "SM.Rasch","SM.2pj","SM.2pjx",
                 "MIMA.Rasch","MIMA.2pj","MIMA.2pjx")
  if (!model %in% modellist) {
    stop("Unknown model detected! Model must be one of these: ", paste(modellist, collapse=", "))
  }
  if (!datatype %in% c("attempts","graded")) {
    stop("Argument \"datatype\" must be specified as \"attempts\" or \"graded\"")
  }
  if (model %in% c("GRM","GPCM")) {
    if (datatype == "attempts") {
      graded.data <- reCodeSM2GRM(data,numAtts)
    } else {
      graded.data <- data
    }
    mirttype <- ifelse(model=="GRM", "graded", "gpcm")
    fitObj <- mirt::mirt(graded.data,model=1,itemtype=mirttype,verbose=FALSE,
        showWarnings = FALSE)
    itemPars <- coef(fitObj,simplify=TRUE,IRTpars=TRUE)$item
    personPars <- mirt::fscores(fitObj, response.pattern=graded.data,  method="EAP")
  }
  else {
    if (datatype == "graded") {
      if (min(data) < 0) stop("There should not be negative values in graded data. Are you sure about the datatype?")
      attempt.data <- reCodeGRM2SM(data,numAtts)
    } else {
      attempt.data <- data
    }
    Xplus <- makeXplus(attempt.data, numAtts); #cat("+")
    Xnaught <- makeXnaught(Xplus, numAtts); #cat("o")
    Xminus <- makeXminus(Xplus, numAtts); #cat("-")

    if (length(grep("SM",model))){
      # cat("configuring SM.")
      mirtdat <- Xnaught
      mirtResponse <- Xnaught
    }
    if (length(grep("MIMA",model))){
      # cat("configuring MIMA.")
      mirtdat <- Xplus
      mirtResponse <- Xminus
    }
    if (length(grep("Rasch",model))){
      mirttype="Rasch"
      # cat("Rasch")
    } else {
      mirttype="2PL"
      # cat("2P")
    }
    if (length(grep("2pj$",model))){
      # TO OBTAIN PARAMETER NAMES, FIT THE STANDARD 2pjx FIRST
        temp.fit <- mirt::mirt(Xnaught,model=1,itemtype="2PL",pars = 'values',
                               verbose=FALSE)
      par.a <- temp.fit$parnum[temp.fit$name=="a1"]
      # MAKE AN INDEX OF PARAMETERS FOR EQUALITY CONSTRAINTS
      mima.eqnum <- list()
      eqnum.index <- cumsum(numAtts)-(numAtts-1)
			for (i in 1:nI){
			  mima.eqnum[[i]] <- par.a[eqnum.index[i]:(eqnum.index[i]+numAtts[i]-1)]
      }
      constrain = mima.eqnum
      # cat("j\n")
    } else {
      constrain = NULL
      # if (length(grep("2pjx",model))) cat("jx\n") else cat("\n")
    }

    fitObj <- mirt::mirt(mirtdat,model=1,itemtype=mirttype,SE=TRUE, constrain=constrain, verbose=FALSE)
    itemPars <- coef(fitObj,simplify=TRUE,IRTpars=TRUE)$item
    personPars <- mirt::fscores(fitObj, response.pattern=mirtResponse, method="EAP")
  }
  return(list(fitObj=fitObj,itemPars=itemPars,personPars=personPars))
}

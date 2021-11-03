
# Functions for simulating person and item PARAMETERS
# i.e. drawing parameters from distributions

simulate.theta <- function(n.person,mu.theta=0,sd.theta=1,seed){
  set.seed(seed)
  theta <- rnorm(n.person,mu.theta,sd.theta)
  return(theta)
}


# simulating from a joint distribution over theta and giveupittude
#' Title
#'
#' @param n.person number of examinees
#' @param type in c("nogiveup","indep","custom") where "nogiveup" sets all giveupitudes to 0, indep samples theta and
#' giveupitude independently, and custom samples from a multivariate normal distribution followed by an exponential transformation
#' for giveupitude.
#' @param mu means vector for theta and giveupitude
#' @param Sigma covariance matrix
#' @param seed random seed
#'
#' @return pparams, a list containing the theta and giveupitude vectors
#'
simulate.multidim <- function(n.person, type="nogiveup", mu=c(0,-3), Sigma=matrix(c(1,-0.45,-0.45,1),nrow=2), seed){
    ## CHANGED THE DEFAULT SIGMA TO ALIGN WITH OUR SIMULATION SETUP (8/9/2016)
  set.seed(seed)
  if (type == "nogiveup") {
    theta <- rnorm(n.person,mu[1],Sigma[1,1])
    giveupitude <- rep(0,n.person)
  } else if (type == "indep") {
    theta <- rnorm(n.person,mu[1],Sigma[1,1])
    giveupitude <- exp(rnorm(n.person,mu[2],Sigma[2,2]))
  } else if (type == "custom") {
    tmp <- mvrnorm(n.person, mu, Sigma)
    theta <- tmp[,1]
    giveupitude <- exp(tmp[,2])
  } else {
    stop("Unknown condition for simulate.multidim! Model must be \"nogiveup\", \"indep\", or \"custom\".")
  }
  pparams <- list(theta=theta,giveupitude=giveupitude)
  return(pparams)
}


####
## these two functions should be superceded by the following *.sm variants
## remove?

simulate.a <- function(n.item,mu.a=0,sd.mu.a=0.4,seed){
  set.seed(seed)
  a <- rlnorm(n.item,meanlog=mu.a,sdlog=sd.mu.a)
  return(a)
}

## IKKYU (3/28): simulate.a WILL BE USED FOR BOTH GRM AND GPCM

simulate.b.legacygrm <- function(n.item,n.category,
                       upper.bound=rep(2,n.item),lower.bound=rep(-2,n.item),
                       steps=rep(0.5,n.item),seed){
    set.seed(seed)
    b <- list()
    for (i in 1:n.item){
        b[[i]] <- NA
        temp.midpoints <- seq(lower.bound[i],upper.bound[i],
                              length=n.category[i]-1)
        for (j in 1:length(temp.midpoints)){
            b[[i]][j] <- runif(n=1,min=temp.midpoints[j]-steps[i],
                               max=temp.midpoints[j]+steps[i])
        }
    }
    return(b)
}
#### IKKYU (2/18/2016): REVISED simulate.b TO ENSURE ENOUGH DISTANCE B/W THRESHOLDS
## (WE'RE NOT USING THIS FOR OUR SIMULATION, SO THIS IS RENAMED AS LEGACY FUNCTION 8/9/2016)
## upper/lower.bound AND steps DETERMINE THE INTERVALS UNIFORM RANDOM VARIABLES ARE DRAWN FROM
### EX) IF LOWER = -2, UPPER = 2, STEP = 0.5 (THESE ARE DEFAULTS), N.CATEGORY = 4, THEN
### THE MIDPOINTS OF THREE INTERVALS ARE c(-2,0,2), AND
### b_1 <- runif(1, -2.5, -1.5); b_2 <- runif(1, -0.5, 0.5); b_3 <- runif(1, 1.5, 2.5)
## TAY ET AL. USED DIFFERENT INTERVALS FOR DIFFERENT ITEMS, SO TO ACCOMMODATE THAT POSSIBILITY
## THE BOUNDS AND STEPS ARE (n.item * 1) VECTORS
### USAGE: test <- simulate.b(n.item=10,n.category=c(rep(3,5),rep(4,5)),seed=218)

simulate.b <- function(n.item,n.category,mu.b=0,sd.mu.b=1,sd.b=1,mindiff=0.15,seed){
  set.seed(seed)
  b <- list()
  for (i in 1:n.item){
    thresholds_satisfied <- FALSE
    while (!thresholds_satisfied) {
      mub <- rnorm(1,mu.b,sd.mu.b)
      b[[i]] <- sort(rnorm(n.category[i]-1,mub,sd.b))
      # We still need to ensure that the results of this are not
      # thresholds that are too close, because that is causing problems for simulated data
      # so make sure minimum distance between thresholds is mindiff
      thresholds_satisfied <- min(abs(c(10,b[[i]]) - c(b[[i]],10))) > mindiff
    }
  }
  return(b)
}

## FOR OUR SIMULATION STUDY, WE'RE USING THE ORIGINAL THRESHOLD GENERATOR (8/9/2016)

## IKKYU (3/28/2016): THRESHOLDS FOR GPCM
simulate.b.gpcm <- function(n.item,n.category,mu.b=0,sd.b=1,seed){
  set.seed(seed)
  b <- list()
  for (i in 1:n.item){
    b[[i]] <- rnorm(n.category[i]-1,mu.b,sd.b)
  }
  return(b)
}


####

#' Title
#'
#' @param n.item
#' @param n.atts
#' @param mu.a
#' @param sd.mu.a
#' @param sd.a
#' @param seed
#'
#' @return a parameters
#'
simulate.a.sm <- function(n.item,n.atts,mu.a=0,sd.mu.a=0.5,sd.a=0.2,seed){
  set.seed(seed)
  a <- list()
  for (i in 1:n.item){
    mua <- rlnorm(1,mu.a,sd.mu.a)
    a[[i]] <- rnorm(n.atts[i],mean=mua,sd=sd.a)
  }
  return(a)
}


#' Title
#'
#' @param n.item
#' @param n.atts
#' @param orderedCats
#' @param mu.b
#' @param sd.mu.b
#' @param sd.b
#' @param seed
#'
#' @return b parameters
#'
simulate.b.sm <- function(n.item,n.atts,
                          orderedCats = FALSE,
                          mu.b = 0, sd.mu.b = 1, sd.b = 0.3,
                          seed){
    ## DEFAULT orderedCats = FALSE BECAUSE WE'RE NOT GENERATING FROM MIMA (8/9/2016)
  set.seed(seed)
  b <- list()
  for (i in 1:n.item){
    mub <- rnorm(1,mu.b,sd.mu.b)
    b[[i]] <- rnorm(n.atts[i],mub,sd.b)
    if (orderedCats) {
      b[[i]] <- sort(b[[i]], decreasing=T)
    }
  }
  return(b)
}


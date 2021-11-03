gpcm.prob <- function(a,b,theta){
    num <- array(NA,c(length(theta),length(b)))
    den <- rep(NA,length(theta))
    for (i in 1:length(theta)){
        for (j in 1:length(b)){
            num[i,j] <- exp(sum(a*(theta[i]-b[1:j])))
            }
        den[i] <- 1+sum(num[i,])
        }
    n.cat <- length(b)+1
    pr.cat <- array(NA, c(length(theta),n.cat))
    for (i in 1:length(theta)){
        for (j in 2:n.cat){
            pr.cat[i,j] <- num[i,j-1]/den[i]
            }
        pr.cat[i,1] <- 1/den[i]
        }
    return(pr.cat)
}

gpcm.score <- function(gpcm.prob,bottom.score){
    score <- rep(NA,nrow(gpcm.prob))
    top.score <- bottom.score+(ncol(gpcm.prob)-1)
    for (i in 1:length(score)){
        score[i] <- sample(bottom.score:top.score,1,prob=gpcm.prob[i,])
    }
    return(score)
}

simulate.NONA.gpcm <- function(a,b,theta,bottom.score,seed){
    n.item <- length(a)
    n.person <- length(theta)
    data.mat <- matrix(NA,n.person,n.item)
    set.seed(seed)
    for (i in 1:n.item){
        success <- FALSE
        while (!success){
            temp.prob <- gpcm.prob(a[i],b[[i]],theta)
            tmp.score <- gpcm.score(temp.prob, bottom.score[i])
            success <- length(unique(tmp.score)) == (length(b[[i]]) + 1)
        }
        data.mat[,i] <- tmp.score
    }
    X <- data.frame(data.mat)
    names(X) <- paste("Q", 1:n.item, sep = "")
    return(X)
}

simulate.gpcm <- function(a, b, theta,
                          omittitude=rep(0,length(theta)),
                          giveupitude=rep(0,length(theta)),
                          bottom.score, seed){
  X <- simulate.NONA.gpcm(a,b,theta,bottom.score,seed)
  #### !!!!!
  numAtts <- rep(3, length(a)) ### WARNING!!! HARD CODED!!!
  ##### !!!!!
  if (sum(omittitude) != 0 | sum(giveupitude) != 0) {
    # post-process
    for (i in 1:nrow(X)) {
      X[i, omittitude[i] > runif(nrow(X))] <- NA
      for (n in 1:(max(numAtts)-1)) {
        # remove a proportion of second, third, etc. attempts
        nextatt <- which(X[i,] == numAtts-n)
        X[i,nextatt[n*giveupitude[i] > runif(length(nextatt))]] <- 0
      }
    }
  }
  return(X)
}


grm.prob <- function(a,b,theta){
  pr <- array(NA,c(length(theta),length(b)))
  for (i in 1:length(theta)){
    for (j in 1:length(b)){
      pr[i,j] <- inv.logit(a*(theta[i]-b[j]))
    }
  }
  n.cat <- length(b)+1
  pr.cat <- array(NA, c(length(theta),n.cat))
  pr.cat[,n.cat] <- pr[,(n.cat-1)]
  for (i in (n.cat-1):2){
    pr.cat[,i] <- pr[,(i-1)]-pr[,i]
  }
  pr.cat[,1] <- 1-apply(pr.cat[,2:n.cat],1,sum)
  return(pr.cat)
}

grm.score <- function(grm.prob,bottom.score){
  score <- rep(NA,nrow(grm.prob))
  top.score <- bottom.score+(ncol(grm.prob)-1)
  for (i in 1:length(score)){
    score[i] <- sample(bottom.score:top.score,1,prob=grm.prob[i,])
  }
  return(score)
}

simulate.grm <- function(a, b, theta,
                         omittitude=rep(0,length(theta)),
                         giveupitude=rep(0,length(theta)),
                         bottom.score, seed){
  # wraps simulate.NONA.grm
  # generates fully observed data using only theta
  # and then post-hoc omits values
  # or sets to 0 values that "would have been given up"
  X <- simulate.NONA.grm(a,b,theta,bottom.score,seed)
  #### !!!!!
  numAtts <- rep(3, length(a)) ### WARNING!!! HARD CODED!!!
  ##### !!!!!
  if (sum(omittitude) != 0 | sum(giveupitude) != 0) {
    # post-process
    # Note: we will still need to make sure that all categories are satistfied
    # after a giveup/omit pass
    success <- FALSE
    while(!success) {
      tmpX <- X
      for (i in 1:nrow(tmpX)) {
        tmpX[i, omittitude[i] > runif(nrow(tmpX))] <- NA
        for (n in 1:(max(numAtts)-1)) {
          # remove a proportion of second, third, etc. attempts
          nextatt <- which(tmpX[i,] == numAtts-n)
          tmpX[i,nextatt[n*giveupitude[i] > runif(length(nextatt))]] <- 0
        }
      }
      ## WARNING!!! HACK ALERT! Next line should really allow for variable attempts
      success	<- mean(apply(tmpX, 2, FUN=function(a){length(unique(a))})) == 4 ## WARNING!!! HACK ALERT!
    }
    X <- tmpX
  }
  return(X)
}

simulate.NONA.grm <- function(a, b, theta, bottom.score, seed){
  # draw from GRM with no missing data
  n.item <- length(a)
  n.person <- length(theta)
  data.mat <- matrix(NA,n.person,n.item)
  set.seed(seed)
  for (i in 1:n.item){
	success	<- FALSE  #avoid degenerate cases where some categories are unobserved
	while(!success) {
   	 temp.prob 	<- grm.prob(a[i],b[[i]],theta)
   	 tmp.score 	<- grm.score(temp.prob,bottom.score[i])
	 success	<- length(unique(tmp.score)) == length(b[[i]])+1
	}
	data.mat[,i]	<- tmp.score
  }
  X <- data.frame(data.mat)
  names(X) <- paste("Q",1:n.item,sep="")
  return(X)
}

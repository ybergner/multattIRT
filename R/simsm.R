#' Simulate Sequential Model
#'
#' An internal function used by \code{\link{multatt.sim}}
#'
#' @param a discrimination parameters; a list of length numItems, where each element is a vector
#' @param b threshold parameters; a list of length numItems, where each element is a vector
#' @param theta student skills; a vector of length numStudents
#' @param omittitude a vector of length numStudents encoding the probability of omitting an item entirely (no attempts made)
#' @param giveupitude a vector of length numStudents encoding the probability of not attempting further after an incorrect attempt
#' @param seed random seed
#' @param debug BOOLEAN flag
#'
#' @return Output is a response matrix of integers, see sample data set \code{\link{responses}}
#'
#'
simulate.sm <- function(a, b, theta,
                        omittitude=rep(0,length(theta)),
                        giveupitude=rep(0,length(theta)),
                        seed, debug=F){
    set.seed(seed)
    numStudents <- length(theta)
    numItems <- length(a)
    X <- matrix(0, numStudents,numItems)
    for (j in 1:numItems) {		##kat (2/5/16): Switched order of loops
        success	<- FALSE
        tries <- 0
        while(!success & tries < 4) {
            for (i in 1:numStudents){
                for (k in 1:length(b[[j]])) { # attempt level
                    if (omittitude[i] > runif(1)) {
                        if (debug) cat("student",i,"is omitting on attempt",k,"of item",j,
                                       ". Current state:",X[i,j],"\n")
                        break
                    }
                    else if (X[i,j] < 0 & giveupitude[i] > runif(1)) {
                        if (debug) cat("student",i,"is giving up on attempt",k,"of item",j,
                                       ". Current state:",X[i,j],"\n")
                        break
                    }
                    else if (inv.logit(a[[j]][k]*(theta[i]-b[[j]][k])) > runif(1)) {
                        X[i,j] <- abs(X[i,j])+1
                        if (debug) cat("student",i,"is correct on attempt",k,"of item",j,
                                       ". Current state:",X[i,j],"\n")
                        break
                    }
                    else {
                        X[i,j] <- X[i,j] - 1
                        if (debug) cat("student",i,"is INCORRECT on attempt",k,"of item",j,
                                       ". Current state:",X[i,j],"\n")
                    }
                }
            }
            tmp	<- X[,j]
            success <- length(unique(tmp[tmp>0])) == length(b[[j]]) # do we need to observe all giveups? I don't think so.
            # success <- length(unique(tmp[tmp!=0])) == 2*length(b[[j]])
            if(!success) {
#               cat("numAtts:",length(b[[j]]),"; observed categories: ", unique(tmp[tmp!=0]), "\n")
#               cat("success = ", success, "for seed ", seed, "; item", j)
#               cat("; a = ", a[[j]], "; b = ", b[[j]], "\n")
#               cat("Perturbing... \n")
              # a[[j]] <- (1-abs(a[[j]][1] - 1)*0.1)*a[[j]]
              # b[[j]] <- (1-abs(b[[j]][1])*0.05)*b[[j]]
              tries <- tries + 1
              ### nooooo!! forgot to reset X[,j]
              X[,j] <- rep(0, numStudents)
            }
            ## WE NEED BOTH + & - FOR EACH ATTEMPT FOR ESTIMATION (8/9/2016)
        }
        if (!success) {
          cat("WARNING: unable to satisfy all categories for seed ", seed, "; item", j, "trying again...\n")
          # stop(paste0("unable to satisfy all categories for seed ", seed, "; item", j))
        }
 }

  if (tries == 4) {
    stop(paste0("FAIL: unable to satisfy all categories for seed ", seed, "; item", j, "after", tries," tries.\n"))
  }  
    
  X[X==0] <- NA
  X <- data.frame(X)
  names(X) <- paste("Q",1:numItems,sep="")
  return(X)
}

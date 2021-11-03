
generateMultAtt <- function(respVec, itemname, attemptsRange = 1:6) {
  newItems = c()
  newU = matrix(NA, length(respVec),length(attemptsRange) )
  for (i in 1:length(attemptsRange)) {
    newItems = c(newItems, paste(itemname,".",attemptsRange[i],sep=""))
    for (j in 1:length(respVec)) {
      if (is.na(respVec[j])) {newU[j,i] <- NA}
      else if (respVec[j] > 0) {newU[j,i] <- (respVec[j] <= attemptsRange[i]) + 0
      } else if (respVec[j] < 0 & abs(respVec[j]) >= attemptsRange[i]) {newU[j,i] <- 0}
      else {newU[j,i] <- NA}
    }
  }
  colnames(newU) <- newItems
  return(newU)
}


#' makeXplus, generate expanded matrix from observed attempt count (OAC) response data.
#' In Xplus, all attempts after correct are imputed to be observed correct
#'
#' @param U, response matrix in OAC format
#' @param allowedAttempts, a vector of length ncol(U) with maximum attempts allowed
#'
#' @return Xplus matrix with ncol = sum(allowedAttempts)
#' @export
#'
#'
makeXplus <- function(U,allowedAttempts) {
  multattmat = c()
  tots = 0
  atts = c()
  for (item in 1:ncol(U)) {
    attrange = seq(1,allowedAttempts[item])
    atts = c(atts,max(attrange))
    tots = tots + max(attrange)

    extrabit <- generateMultAtt(U[,item], colnames(U)[item], attrange)
    multattmat <- cbind(multattmat,extrabit)
  }
  rownames(multattmat) <- rownames(U)
  multattmat
}

#' makeXnaught, generate standard sequential matrix from expanded (Xplus) response data
#' In Xnaught, all attempts after correct are unobserved

#' @param responses, Xplus matrix
#' @param attempts, a vector of length = numItems, maximum attempts allowed
#'
#' @return Xnaught matrix
#' @export
#'
#'
makeXnaught <- function(responses, attempts) {
  #### Loop over examinees, i,
  for(i in 1:nrow(responses))
  {
    itemStart <- 1
    #### Loop over real items,j, based on length of attempts matrix
    for(j in 1:length(attempts))
    {
      numAttempts <- attempts[j]				# read from attempt vector
      hardestAttemptCorrect   <- match(responses[i,(itemStart:(itemStart - 1 + numAttempts))],x=1)
      if(!(is.na(hardestAttemptCorrect)) & hardestAttemptCorrect < numAttempts)
      {
        # set all attempts AFTER the 1 to NA
        for (k in 1:(numAttempts-hardestAttemptCorrect)) {
          responses[i,(itemStart+hardestAttemptCorrect+k-1)] <- NA
        }

      }
      # 		print(c(i,j,itemStart))
      itemStart <- itemStart + numAttempts
    }

  }
  return(responses)

}

#' makeXminus, generate reduced (Xminus) matrix from expanded (Xplus) response data
#' In Xminus, all but the last incorrect attempt and/or first correct attempt are unobserved
#'
#' @param responses, Xplus matrix
#' @param attempts, a vector of length = numItems, maximum attempts allowed
#'
#' @return Xplus matrix
#' @export
#'
#'

makeXminus <- function(responses, attempts) {
  #### Loop over examinees, i,
  for(i in 1:nrow(responses))
  {
    itemStart <- 1
    #### Loop over real items,j, based on length of attempts matrix
    for(j in 1:length(attempts))
    {
      numAttempts <- attempts[j]				# read from attempt vector
      hardestAttemptCorrect   <- match(responses[i,(itemStart:(itemStart - 1 + numAttempts))],x=1)
      easiestAttemptIncorrect <- match(responses[i,((itemStart - 1 + numAttempts):itemStart)],x=0)
      madeAttempt <-!(is.na(easiestAttemptIncorrect))

      ####	Set all "items" within current item to 9 (or NA)
      responses[i,(itemStart:(itemStart -1 + numAttempts))] <- NA

      #### if item is correct on some attempt:
      #### set earliest attempt correct (hardest item) to 1
      #### set preceding attempt to 0, unless earliest attempt
      #### correct is on 1st try
      if(!(is.na(hardestAttemptCorrect)))
      {
        responses[i,(hardestAttemptCorrect+itemStart-1)] <- 1
        if (hardestAttemptCorrect != 1)
          responses[i,(hardestAttemptCorrect+itemStart-2)] <- 0
      } else if(madeAttempt)
      {
        #### if did not get correct (hardestAttemptCorrect = NA)
        #### and made at least 1 attempt, then give 0 for "easiest" item,
        #### give 0 for "easiest" item, other "items" are already 9 (or NA)
        #### otherwise all entries are already 9s (or NAs)
        responses[i,itemStart+numAttempts-easiestAttemptIncorrect] <- 0
      }
      # 		print(c(i,j,itemStart))
      itemStart <- itemStart + numAttempts
    }
  }
  return(responses)
}

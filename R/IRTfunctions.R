
########

reshape.params <- function(param.vec) {
	X = matrix(param.vec[1:(numItems*2)],numItems,2)
	Theta = matrix(param.vec[(numItems*2+1):length(param.vec)],numStudents,1)
	Theta = cbind(rep(1,numStudents),Theta)
	return(list(X=X,Theta=Theta))
}
########

sigmoid <- function(z) {
	1/(1+exp(-z))
}

logitRasch <- function(sk, d) {
	sigmoid(sk-d)
}

logistic2pl <- function(sk, d, a) {     # takes scalar values, returns scalar value (element-wise)
	sigmoid(a*(sk-d))
	}

logistic2pl.vec <- function(sk, d, a) {
        ### uses vectors and returns matrix
	smat = matrix(rep(sk,length(d)),nrow=length(sk))
	dmat = matrix(rep(d,length(sk)),nrow=length(sk),byrow=T)
	amat = matrix(rep(a,length(sk)),nrow=length(sk),byrow=T)
	sigmoid(amat * (smat - dmat))
}


#######################################################

SI2DD <- function(X1,X2,noerrs=TRUE) {
	### to prevent div by zero errors
	if (noerrs) {
	X2[which(abs(X2) < 0.01),1] <- 0.01
	diffs = -X1/X2
	list(diffs = diffs, discrims=X2)
	} else {
	X2[which(abs(X2[,1]) < 0.001),1] <- 0.001
	diffs = -X1[,1]/X2[,1]
	diffs.se = sqrt((X2[,1]^2*X1[,2]^2 + X1[,1]^2*X2[,2]^2)/X2[,1]^4)
	list(diffs = cbind(diffs,diffs.se), discrims=X2)
	}
}


#######################################################

DD2SI <- function(Diff, Discrim) {
	X1 = -Diff[,1]*Discrim[,1]
	X1.se = sqrt(Discrim[,1]^2*Diff[,2]^2 + Diff[,1]^2*Discrim[,2]^2)
	list(X1=cbind(X1,X1.se), X2=Discrim)
}


#######################################################

RescaleDD <- function(Theta, Diff, Discrim, noerrs=FALSE) {
	sig = sd(Theta)
	mu = mean(Theta)
    if (noerrs) {
    Theta <- (Theta-mu)/sig
	Diff <- (Diff-mu)/sig
	Discrim <- sig*Discrim
	list(Theta=Theta, Diff=Diff, Discrim=Discrim)
    } else {
	Theta[,1] <- (Theta[,1]-mu)/sig
	Theta[,2] <- Theta[,2]/sig
	Diff[,1] <- (Diff[,1]-mu)/sig
	Diff[,2] <- Diff[,2]/sig
	Discrim[,1] <- sig*Discrim[,1]
	Discrim[,2] <- sig*Discrim[,2]
	list(Theta=Theta, Diff=Diff, Discrim=Discrim)
	}
}

#######################################################

RescaleSI <- function(Theta, X1, X2, noerrs=FALSE, tilt=F) {
	if (noerrs) {
		if (!tilt & mean(X2) < 0) {
			X2 = -X2
			Theta = -Theta
		}
		sig = sd(Theta)
		mu = mean(Theta)
		Theta <- (Theta-mu)/sig
		X1 <- X1 + mu*X2
		X2 <- sig*X2
		return(list(Theta=Theta, X1=X1, X2=X2))
	} else {
		if (!tilt & mean(X2[,1]) < 0) {
			X2[,1] = -X2[,1]
			Theta[,1] = -Theta[,1]
		}
		sig = sd(Theta[,1])
		mu = mean(Theta[,1])
		Theta[,1] <- (Theta[,1]-mu)/sig
		Theta[,2] <- Theta[,2]/sig
		X1[,1] <- X1[,1] + mu*X2[,1]
		X1[,2] <- sqrt(X1[,2]^2 + (mu*X2[,2])^2)
		X2[,1] <- sig*X2[,1]
		X2[,2] <- sig*X2[,2]
		return(list(Theta=Theta, X1=X1, X2=X2))
	}
}


#######################################################
RescaleSI.noerrs <- function(Theta, X1, X2) {
	if (mean(X2) < 0) {
		X2 = -X2
		Theta = -Theta
	}
	sig = sd(Theta)
	mu = mean(Theta)
	Theta <- (Theta-mu)/sig
	X1 <- X1 + mu*X2
	X2 <- sig*X2

	list(Theta=Theta, X1=X1, X2=X2)

}


#######################################################

logistic3pl <- function(sk, d, a, g) {
	g + (1-g)*logistic2pl(sk,d,a)
}


#######################################################

logLikelihood <- function(theta, ResponseVector, ItemDifficulty, ItemDiscrim) {
    logLikelihood = 0
    numItems = length(ItemDifficulty)
	for(i in 1:numItems) {
			if (!is.na(ResponseVector[i])) {
				P = logistic2pl(theta, ItemDifficulty[i], ItemDiscrim[i])
				logLikelihood = logLikelihood  + ResponseVector[i]*log(P) + (1-ResponseVector[i])*log(1 - P)
			}
	}
	logLikelihood
}
#######################################################



row.isna <- function(matrix) {
  apply(matrix,1,FUN=function(row){length(which(is.na(row)))})
}

col.isna <- function(matrix) {
  apply(matrix,2,FUN=function(col){length(which(is.na(col)))})
}

col.iscft <- function(matrix) {
  apply(matrix,2,FUN=function(col){length(which(col==1))})
}

col.cftfrac <- function(matrix) {
  apply(matrix,2,FUN=function(col){length(which(col==1))/(nrow(matrix)-length(which(is.na(col))))})
}


#######################################################


#######################################################
# allows using a different number of attempts for each item (column) of a response matrix
# takes the matrix U and a vector called itemAllowedAttempts

makeDichotomous <- function(U,itemAllowedAttempts) {
	for (i in 1:ncol(U)) {
		U[which(U[,i] > itemAllowedAttempts[i] | U[,i] < 0),i] <- 0
		if (itemAllowedAttempts[i] > 1) {
   			for (n in 2:itemAllowedAttempts[i]) {
				U[which(U[,i] == n),i] <- 1
  			}
		}
	}
	return(U)
}


#########################################
scrapematrix <- function(U, remove="both", sparseFraction, quiet=FALSE, removeAbsolutes=TRUE) {

removeCols = FALSE
removeRows = FALSE
if (remove=="both" | remove == "cols") {
  removeCols = TRUE
}
if (remove == "both" | remove == "rows") {
  removeRows = TRUE
}

srows <- which(row.isna(U)/ncol(U) > sparseFraction)
scols <- which(col.isna(U)/nrow(U) > sparseFraction)
sr = length(srows)
sc = length(scols)

SPARSE = as.logical(sr*(removeRows+0) + sc*(removeCols+0))

if (sr > nrow(U)-5 | sc > ncol(U)-2) {
	cat('needed to remove too many rows or columns; exiting with first row \n')
	return(as.matrix(U[1,]))
}
while (SPARSE) {
    if (length(srows) & removeRows) {
    	if (!quiet) cat('removing',length(srows),'rows which are more than',sparseFraction*100,'% sparse \n')
    	U = U[-srows,]
    }
	scols <- which(col.isna(U)/nrow(U) > sparseFraction)
    if (length(scols) & removeCols) {
    	if (!quiet) cat('removing',length(scols),'columns which are more than',sparseFraction*100,'% sparse \n')
        U = U[,-scols]
    }
	srows <- which(row.isna(U)/ncol(U) > sparseFraction)
	SPARSE = as.logical(length(srows)*(removeRows+0) + length(scols)*(removeCols+0))
}

if (removeAbsolutes) {
    PersonProportionCorrect <- rowSums(U, na.rm=TRUE)/rowSums(!is.na(U))
    ItemProportionCorrect <- colSums(U, na.rm=TRUE)/colSums(!is.na(U))
    toogood= which(PersonProportionCorrect==1)
    toobad= which(PersonProportionCorrect==0)
    tooeasy= which(ItemProportionCorrect==1)
    toohard= which(ItemProportionCorrect==0)

    # cat('toogood:',length(toogood),'toobad:',length(toobad),'toohard:',length(toohard),'tooeasy:',length(tooeasy),'\n')
    # cat('toohard items:',colnames(U)[toohard],'\n')

    while (nrow(U) > 5 & ncol(U) > 5 & as.logical(length(toogood)+length(toobad)+length(toohard)+length(tooeasy))) {

        if (length(toobad)) {
            U = U[-toobad,]
            if (!quiet) cat('removing',length(toobad), 'students who got all items wrong \n')
            }
        if (length(toogood)) {
            U = U[-toogood,]
            if (!quiet) cat('removing',length(toogood), 'students who got all items correct \n')
            }
        if (length(tooeasy)) {
            U = U[,-tooeasy]
            if (!quiet) cat('removing',length(tooeasy), 'items which were marked all correct \n')
            }
        if (length(toohard)) {
            U = U[,-toohard]
            if (!quiet) cat('removing',length(toohard), 'items which were marked all wrong \n')
            }

        PersonProportionCorrect <- rowSums(U, na.rm=TRUE)/rowSums(!is.na(U))
        ItemProportionCorrect <- colSums(U, na.rm=TRUE)/colSums(!is.na(U))
        toogood= which(PersonProportionCorrect==1)
        toobad= which(PersonProportionCorrect==0)
        tooeasy= which(ItemProportionCorrect==1)
        toohard= which(ItemProportionCorrect==0)
    # cat('toogood:',length(toogood),'toobad:',length(toobad),'toohard:',length(toohard),'tooeasy:',length(tooeasy),'\n')
    # cat('toohard items:',colnames(U)[toohard],'\n')
    }
}

if (nrow(U) <= 5 | ncol(U) <= 5) {
	cat('needed to remove too many rows or columns; exiting with first row \n')
	return(as.matrix(U[1,]))
}
return(U)

}


scraperowsonly <- function(U, sparseFraction, quiet=FALSE, removeAbsolutes=TRUE) {

srows <- which(row.isna(U)/ncol(U) > sparseFraction)
sr = length(srows)
if (sr > nrow(U)-5) {
	cat('needed to remove too many rows or columns; exiting with first row \n')
	return(as.matrix(U[1,]))
}
    if (length(srows)) {
    	if (!quiet) cat('removing',length(srows),'rows which are more than',sparseFraction*100,'% sparse \n')
    	U = U[-srows,]
    }


if (removeAbsolutes) {
    PersonProportionCorrect <- rowSums(U, na.rm=TRUE)/rowSums(!is.na(U))
    toogood= which(PersonProportionCorrect==1)
    toobad= which(PersonProportionCorrect==0)

    while (nrow(U) > 5 & ncol(U) > 5 & as.logical(length(toogood)+length(toobad))) {

        if (length(toobad)) {
            U = U[-toobad,]
            if (!quiet) cat('removing',length(toobad), 'students who got all items wrong \n')
            }
        if (length(toogood)) {
            U = U[-toogood,]
            if (!quiet) cat('removing',length(toogood), 'students who got all items correct \n')
            }

        PersonProportionCorrect <- rowSums(U, na.rm=TRUE)/rowSums(!is.na(U))
        toogood= which(PersonProportionCorrect==1)
        toobad= which(PersonProportionCorrect==0)
    }
}

if (nrow(U) <= 5 | ncol(U) <= 5) {
	cat('needed to remove too many rows or columns; exiting with first row \n')
	return(as.matrix(U[1,]))
}
return(U)

}


###############
medianattempts <- function(U, maxAttempts=99, useAbs = F) {

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

############
logitstandardize <- function(distro) {
    maxd <- max(distro)
    distro <- distro/maxd
    distro[which(distro==0)] <- 0.333*1/maxd
    distro[which(distro==1)] <- 1-0.333*1/maxd
    distro <- log(distro/(1-distro))
    distro <- scale(distro)
    return(distro)
}

##########################
##### END FUNCTIONS ######
##########################

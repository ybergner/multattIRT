pcm.prob <- function(theta, scale, delta){
    pcm.num <- exp(matrix(seq(0,length(delta),1),ncol=1) %*% matrix(scale * theta,nrow=1) -
            matrix(rep(c(0, cumsum(delta)), length(theta)), nrow = length(delta)+1))
    pcm.den <-matrix(rep(colSums(pcm.num),length(delta)+1), nrow = length(delta)+1, byrow=TRUE)
    return(pcm.num/pcm.den)
}

sm.prob <- function(theta, beta){
    sm.num <- exp(matrix(seq(0,length(beta),1),ncol=1) %*% matrix(theta,nrow=1) -
                      matrix(rep(c(0, cumsum(beta)), length(theta)), nrow = length(beta)+1))
    comp.mat <- rbind(1+exp(sapply(theta,"-",beta)),rep(1,length(theta)))
    sm.den <- apply(comp.mat, 2, cumprod)
    return(sm.num/sm.den)
    }

# CGRM STANDS FOR CONSTRAINED GRM (WITHOUT ITEM-SPECIFIC SLOPES)
cgrm.prob <- function(theta, scale, delta){
    cgrm.higher <- 1/(1+exp(-(
        matrix(rep(1, length(delta)), ncol=1) %*% matrix(scale * theta, nrow = 1) -
        matrix(rep(delta, length(theta)), nrow = length(delta)))))
    cgrm.comp <- rbind(rep(1, length(theta)), cgrm.higher)
    cgrm.irf <- rbind(-diff(cgrm.comp), cgrm.comp[length(delta)+1,])
    return(cgrm.irf)
}

sm.pcm.loss <- function(theta, params, beta, score){
    pcm.num <- exp(
        matrix(seq(0,length(params)-1,1),ncol=1) %*% matrix(params[1] * theta,nrow=1) -
        matrix(rep(c(0, cumsum(params[2:length(params)])), length(theta)), nrow = length(params))
        )
    pcm.den <-matrix(rep(colSums(pcm.num),length(params)), nrow = length(params), byrow=TRUE)
    sm.num <- exp(
        matrix(seq(0,length(beta),1),ncol=1) %*% matrix(theta,nrow=1) -
        matrix(rep(c(0, cumsum(beta)), length(theta)), nrow = length(beta)+1)
        )
    comp.mat <- rbind(1+exp(sapply(theta,"-",beta)),rep(1,length(theta)))
    sm.den <- apply(comp.mat, 2, cumprod)
    sm.pcm.diff <- (sm.num[score+1,]/sm.den[score+1,])-(pcm.num[score+1,]/pcm.den[score+1,])
    output <- (sm.pcm.diff^2) * dnorm(theta)#(1/sqrt(2*pi)) * exp((-theta^2)/2)
    return(output)
}

sm.pcm.sumint <- function(params, beta, max.score, floor, ceiling){
    results <- rep(NA, max.score+1)
    for (i in 1:(max.score+1)){
        temp <- integrate(sm.pcm.loss, lower = floor, upper = ceiling,
                          params = params, beta = beta, score = i-1)
        results[i] <- temp$value
    }
    return(sum(results))
}

sm.cgrm.loss <- function(theta, params, beta, score){
    cgrm.higher <- 1/(1+exp(-(
        matrix(rep(1, length(params)-1), ncol=1) %*% matrix(params[1] * theta, nrow = 1) -
        matrix(rep(params[2:length(params)], length(theta)), nrow = length(params)-1))))
    cgrm.comp <- rbind(rep(1, length(theta)), cgrm.higher)
    cgrm.irf <- rbind(-diff(cgrm.comp), cgrm.comp[length(params),])
    sm.num <- exp(
        matrix(seq(0,length(beta),1),ncol=1) %*% matrix(theta,nrow=1) -
        matrix(rep(c(0, cumsum(beta)), length(theta)), nrow = length(beta)+1)
        )
    comp.mat <- rbind(1+exp(sapply(theta,"-",beta)),rep(1,length(theta)))
    sm.den <- apply(comp.mat, 2, cumprod)
    sm.cgrm.diff <- (sm.num[score+1,]/sm.den[score+1,])-cgrm.irf[score+1,]
    output <- (sm.cgrm.diff^2) * dnorm(theta)
    return(output)
}

sm.cgrm.sumint <- function(params, beta, max.score, floor, ceiling){
    results <- rep(NA, max.score+1)
    for (i in 1:(max.score+1)){
        temp <- integrate(sm.cgrm.loss, lower = floor, upper = ceiling,
                          params = params, beta = beta, score = i-1)
        results[i] <- temp$value
    }
    return(sum(results))
}

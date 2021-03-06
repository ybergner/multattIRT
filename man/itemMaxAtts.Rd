% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itemMaxAtts.r
\name{itemMaxAtts}
\alias{itemMaxAtts}
\title{itemMaxAtts is used to obtain a vector of maximum attempts based on a target proportion correct}
\usage{
itemMaxAtts(U, targetProp = 0.85, hardMax = 4)
}
\arguments{
\item{U}{a response matrix in OAC format}

\item{targetProp}{proportion correct per item above which no more attempts will be counted (default 0.85)}

\item{hardMax}{an upper limit on attempts regardless of proportion (default 4)}
}
\value{
a vector of attempts of length = ncol(U)
}
\description{
itemMaxAtts is used to obtain a vector of maximum attempts based on a target proportion correct
}


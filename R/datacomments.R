#' Responses
#'
#' A dataset containing the observed attempt counts (OAC) of 100 students to 10 items.
#' The OAC format contains integer values as well as missing responses, which are coded as NA.
#' Positive integers m refer to the (last) observed attempt which was correct.
#' Negative integers n indicate that the last observed attempt abs(n) was incorrect.
#' This data set is randomly sampled from a large MOOC data set.
#'
#' @format A data frame with 100 rows (students) and 10 items:
#' \describe{
#'   \item{rows}{student response vectors}
#'   \item{columns}{items labeled Q1, Q2}
#'   ...
#' }
"responses"

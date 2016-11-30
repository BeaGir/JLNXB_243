#' @title summary of eigenvalues
#' @description method for summarizing information about the obtained eigenvalues
#' @param x the mfa object created by the mfa function
#' @return a dataframe with singular values, eigenvalues, cumulative, percentage of intertia, cumulative percentage of inertia, for all the extracted components
#' @examples
#'
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' mymfa1 <- mfa(wines, sets, ncomps = 2, T, scaling_vec)
#'
#' eigensummary(mymfa1)
#' @export
eigensummary <- function(x) UseMethod("eigensummary")
#' @export
eigensummary.mfa <- function(x) {
  Singular_value <- round(sqrt(x$eigen_values), 3)
  Eigenvalue <- round(x$eigen_values, 3)
  Cumulative <- cumsum(Eigenvalue)
  Percent_Inertia <- round(100/Cumulative[length(Cumulative)] * Eigenvalue)
  Cumulative_inertia <- cumsum(Percent_Inertia)
  summary_matrix <- rbind(Singular_value, Eigenvalue, Cumulative, Percent_Inertia, Cumulative_inertia)
  output_matrix <- data.frame(summary_matrix)
  output_matrix
}

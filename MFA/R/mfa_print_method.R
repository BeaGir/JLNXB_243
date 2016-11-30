#' @title print method of object mfa
#' @description print the basic information of a mfa object
#' @param x an object of class \code{"mfa"}
#' @examples
#'
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' mymfa1 <- mfa(wines, sets, ncomps = 2, T, scaling_vec)
#'
#' mymfa1
#' @export
print.mfa <- function(x) {
  cat('object of class "mfa"\n')
  cat('number of observations:', nrow(x$common_factor_scores), "\n")
  cat('number of active variables:', nrow(x$loadings), "\n")
  cat('number of data tables:', length(x$partial_factor_scores), "\n")
  cat('number of components called:', ncol(x$loadings), "\n")
  cat('Eigenvalues (max of 2):', round(x$eigen_values[1:min(2, length(x$eigen_values))], 3), "\n")
  cat('Loadings (dimension 1):', round(x$loadings[,1], 3), "\n")
  invisible(x)
}

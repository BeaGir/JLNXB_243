#' @title contrib_obs
#' @description Computes the contribution of each observation to each dimension \code{"contrib_obs"}
#' @param mfaex, an object of class "mfa"
#' @return the matrix of contributions M[i,l] is the contribution of observation i to dimension l
#' @export
#' @examples
#' 
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' mymfa1 <- mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)
#' 
#' contrib_obs1 <- contrib_obs(mymfa1)

source('Project_Concat.R')

contrib_obs <- function(mfaex){
  # INPUT
  # An mfa object
  
  # OUTPUT
  # A matrix containing the contribution of each observation to the components
  # ctr[i,l] = contrib. wine i to the component l
  
  Factor = mfaex$common_factor_scores
  # Number of observations
  i = nrow(Factor)
  # Mass of each observations
  m = 1/i
  # Number of components
  l = ncol(Factor)
  # Eigenvalues related to each component
  lambda = matrix(mfaex$eigen_values[1:l], nrow = i, ncol = l, byrow = TRUE)
  
  # We compute the contributions and store it in a matrix.
  ctr = (m*Factor^2)/lambda
  ctr
}

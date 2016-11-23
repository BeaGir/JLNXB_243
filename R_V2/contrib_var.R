#' @title contrib_var
#' @description Computes the contribution of each variable to each dimension \code{"contrib_var"}
#' @param mfaex, an object of class "mfa"
#' @return the matrix of contributions M[j,l] is the contribution of variable k to dimension l
#' @export
#' @examples
#' 
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' tables <- data_tables(wines, sets, TRUE, scaling_vec)
#' mymfa1 <- mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)
#' 
#' contrib_var1 <- contrib_var(mymfa1, sets, tables)


contrib_var <- function(mfaex, setex, tables){
  # INPUT
  # An mfa object
  # The sets of indices defining the tables
  # The pre-processed tables
  
  # OUTPUT
  # A matrix containing the contribution of each variables to the components
  # ctr[j,l] = contrib. var j to the component l
  
  # We define Q, F and the partial F matrices.
  Q = mfaex$loadings
  Factor = mfaex$common_factor_scores
  Ftables = mfaex$partial_factor_scores
  
  # Dimensions
  j = nrow(Q)
  l = ncol(Q)
  
  # Tables
  sets = lapply(setex, FUN=function(vec){vec-1})
  K = length(sets)
  
  
  # Weights of each variable from eq 22
  alpha = rep(1,j)
  
  for (k in 1:K){
    # We extract the k-th table
    # Xk = X[,sets[[k]]]
    Xk = tables[[k]]
    XQk = K*Xk %*% Q[sets[[k]],]
    
    alpha[sets[[k]]] <- rep((Ftables[[k]][1,1])/XQk[1,1], ncol(Xk))
  }
  
  # Matrix alpha size (j,l), same value on a row for all columns
  alpha = matrix(alpha, nrow = j, ncol = l, byrow = FALSE)
  
  # We compute the contributions and store it in a matrix.
  ctr = (Q^2)*alpha
  ctr
}
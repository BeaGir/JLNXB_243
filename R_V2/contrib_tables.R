#' @title contrib_tables
#' @description Computes the contribution of each table to each dimension \code{"contrib_tables"}
#' @param mfaex, an object of class "mfa"
#' @return the matrix of contributions M[k,l] is the contribution of table k to dimension l
#' @export
#' @examples
#' 
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' tables <- data_tables(wines, sets, TRUE, scaling_vec)
#' mymfa1 <- mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)
#' 
#' contrib_table1 <- contrib_tables(mymfa1, sets, tables)


contrib_tables <- function(mfaex, setex, tables){
  # We compute the contribution of each variables
  ctrvar = contrib_var(mfaex, setex, tables)
  
  sets = lapply(setex, FUN=function(vec){vec-1})
  K = length(tables)
  L = ncol(ctrvar)
  
  # We sum the contribution in each tables for each dimension
  ctrtables = matrix(1,K,L)
  
  for (k in 1:K){
    ctrtables[k,] = colSums(ctrvar[sets[[k]],])
  }
  
  ctrtables
}

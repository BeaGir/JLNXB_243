#' @title contrib_obs
#' @description Computes the contribution of each observation to each dimension \code{"contrib_obs"}
#' @param mfaex, an object of class "mfa"
#' @return the matrix of contributions M[i,l] is the contribution of observation i to dimension l
#' @examples
#'
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' mymfa1 <- mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)
#'
#' contrib_obs1 <- contrib_obs(mymfa1)
#' @export
contrib_obs <- function(mfaex){
  # INPUT
  # An mfa object
  # OUTPUT
  # A matrix containing the contribution of each observation to the components
  # ctr[i,l] = contrib. wine i to the component l

  check_mfa(mfaex)

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

check_mfa <- function(mfaex){
  if(!(class(mfaex) == "mfa")){
    stop("\n 'mfaex must be an object of class mfa")
  }
  TRUE
}






#' @title contrib_var
#' @description Computes the contribution of each variable to each dimension \code{"contrib_var"}
#' @param mfaex, an object of class "mfa" builded from an original data-set containing the 'tables'
#' @param tables, some original tables
#' @param sets, the sets of variables used in the MFA (mapping between the tables and the mfa variables)
#' @return the matrix of contributions for all variables : M[k,l] is the contribution of variable k to dimension l
#' @examples
#'
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' tables <- data_tables(wines, sets, TRUE, scaling_vec)
#' mymfa1 <- mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)
#'
#' sets <- lapply(sets, FUN=function(vec){vec-1})
#' contrib_var1 <- contrib_var(mymfa1, sets, tables)
#' @export

contrib_var <- function(mfaex, sets, tables){
  # INPUT
  # An mfa object, builded from the given tables
  # The sets of indices defining the tables
  # The pre-processed tables
  # OUTPUT
  # A matrix containing the contribution of each variables to the components
  # ctr[j,l] = contrib. var j to the component l

  check_contrib(mfaex, sets, tables)

  # We define Q, F and the partial F matrices.
  Q = mfaex$loadings
  Factor = mfaex$common_factor_scores
  Ftables = mfaex$partial_factor_scores

  # Dimensions
  j = nrow(Q)
  l = ncol(Q)

  # Tables
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

check_contrib <- function(mfaex, sets, tables){
  if(!(class(mfaex) == "mfa")){
    stop("\n 'mfaex must be an object of class mfa")
  }
  if (!(is.list(sets))){
    stop("\n 'sets' must be a list")
  }
  if (!(is.list(tables))){
    stop("\n 'tables' must be a list")
  }
  if (length(sets) != length(tables)){
    stop("\n 'sets' and 'tables' must have the same number of elements")
  }
  for (i in 1:length(sets)){
    if (!is.numeric(sets[[i]])){
      stop("\n 'sets' must be a list of numeric indices")
    }
    if (!(is.numeric(tables[[i]]))){
      stop("\n 'tables' must contain only numeric elements")
    }
    if ((min(sets[[i]] <= 0)) | (max(sets[[i]]) > nrow(mfaex$loadings))){
      stop("\n 'sets' must be consistent with the number of variables in the mfa")
    }
    if (length(sets[[i]]) != ncol(tables[[i]])){
      stop("\n 'sets' must be a consistent mapping of the tables columns to the mfa variables.")
    }
  }
  TRUE
}






#' @title contrib_tables
#' @description Computes the contribution of each table to each dimension \code{"contrib_tables"}
#' @param mfaex, an object of class "mfa" builded from an original data-set containing the 'tables'
#' @param tables, some original tables
#' @param setex, the sets of variables used in the MFA (mapping between the tables and the mfa variables)
#' @return the matrix of contributions M[k,l] is the contribution of table k to dimension l
#' @examples
#'
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' tables <- data_tables(wines, sets, TRUE, scaling_vec)
#' mymfa1 <- mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)
#'
#' sets <- lapply(sets, FUN=function(vec){vec-1})
#' contrib_table1 <- contrib_tables(mymfa1, sets, tables)
#' @export

contrib_tables <- function(mfaex, setex, tables){

  check_contrib(mfaex, setex, tables)

  # We compute the contribution of each variables
  ctrvar = contrib_var(mfaex, setex, tables)

  K = length(tables)
  L = ncol(ctrvar)

  # We sum the contribution in each tables for each dimension
  ctrtables = matrix(1,K,L)

  for (k in 1:K){
    ctrtables[k,] = colSums(ctrvar[setex[[k]],])
  }

  ctrtables
}


check_contrib <- function(mfaex, sets, tables){
  if(!(class(mfaex) == "mfa")){
    stop("\n 'mfaex must be an object of class mfa")
  }
  if (!(is.list(sets))){
    stop("\n 'sets' must be a list")
  }
  if (!(is.list(tables))){
    stop("\n 'tables' must be a list")
  }
  if (length(sets) != length(tables)){
    stop("\n 'sets' and 'tables' must have the same number of elements")
  }
  for (i in 1:length(sets)){
    if (!is.numeric(sets[[i]])){
      stop("\n 'sets' must be a list of numeric indices")
    }
    if (!(is.numeric(tables[[i]]))){
      stop("\n 'tables' must contain only numeric elements")
    }
    if ((min(sets[[i]] <= 0)) | (max(sets[[i]]) > nrow(mfaex$loadings))){
      stop("\n 'sets' must be consistent with the number of variables in the mfa")
    }
    if (length(sets[[i]]) != ncol(tables[[i]])){
      stop("\n 'sets' must be a consistent mapping of the tables columns to the mfa variables.")
    }
  }

  TRUE
}



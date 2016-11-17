# COMMENTS
# contrib_obs returns the contributions of each observation to the dimensions
# contrib_var returns teh contribution of each variable
# contrib_tables returns the contribution of each table

# QUESTION
# tables != mymfa$common_factor_scores * t(mymda$loadings), WHY?

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
  
  # We re-create the data matrix X
  # X = Factor %*% t(Q)
  
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



contrib_tables <- function(mfaex, setex, tables){
  ctrvar = contrib_var(mfaex, setex, tables)
  
  sets = lapply(setex, FUN=function(vec){vec-1})
  K = length(tables)
  L = ncol(ctrvar)
  
  ctrtables = matrix(1,K,L)
  
  for (k in 1:K){
    ctrtables[k,] = colSums(ctrvar[sets[[k]],])
  }
  
  ctrtables
}

contribution_obs = contrib_obs(mymfa)
contribution_var = contrib_var(mymfa, sets, tables)
contribution_tables = contrib_tables(mymfa, sets, tables)

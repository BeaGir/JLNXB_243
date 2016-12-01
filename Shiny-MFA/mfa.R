
#' @title mfa (main function)
#' @description Creates an object of class \code{"mfa"}
#' @param data data set(a dataframe or matrix)
#' @param sets a list of vectors indicating sets/blocks of variables, can be character vectors with names or numeric vectors with position of variables in the data table
#' @param ncomps an integer indicating how many components/factors are to be extracted, NULL indicates all components
#' @param center logical value or numeric vector of length equal to number of active variables; if numeric vector, each variable has corresponding value subtracted from it; if TRUE, subtract column means
#' @param scale logical value or numeric vector of length equal to number of active variables
#' @return an object of class mfa
#' @examples
#'
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' mymfa1 <- mfa(wines, sets, ncomps = 2, T, scaling_vec)
#' @export
mfa <- function(data, sets, ncomps = NULL, center = TRUE, scale = TRUE) {
  tables <- data_tables(data, sets, center, scale)
  pca <- pca_func(tables, sets, ncomps)
  make_mfa(data, pca, ncomps)
}



#helper function for function that separates data into tables and preprocesses
shift_sets <- function(sets) {
  data_start_col <- min(unlist(sets))
  shift <- data_start_col - 1
  new_sets <- lapply(sets, function(x) x - shift)
  return(new_sets)
}



#' @title data_tables
#' @description separate data into individual tables, preprocess and store in a list
#' @param data data set(a dataframe or matrix)
#' @param sets a list of vectors indicating sets/blocks of variables, can be character vectors with names or numeric vectors with position of variables in the data table
#' @param center logical value or numeric vector of length equal to number of active variables; if numeric vector, each variable has corresponding value subtracted from it; if TRUE, subtract column means
#' @param scale logical value or numeric vector of length equal to number of active variables
#' @return a list which contains all tables of the dataset
#' @examples
#'
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' tables <- data_tables(wines, sets, TRUE, scaling_vec)
#' @export
data_tables <- function(data, sets, center, scale) {
  new_sets <- shift_sets(sets)
  raw_tables <- lapply(sets, function(x) {subset(data, select = x)})
  if(class(scale) == "logical") {
    tables <- lapply(raw_tables, function(x) {scale(x, center = center, scale = scale)})
  } else {
    index <- 1:length(new_sets)
    tables <- mapply(function(x, i) {scale(x, center = center, scale = scale[new_sets[[i]]])}, raw_tables, index)
  }
  return(tables)
}



#determine the weights for each individual table
weights <- function(list) {
  #create a nested list with the svd matrices for each data table
  svd_list <- lapply(list, svd)
  #create vector of weights
  wghts <- unlist(lapply(svd_list, function(x) {1/(x$d[1])^2}))
  return(wghts)
}



#create matrix of weightings
weighting_matrix <- function(weights, sets) {
  y <- lapply(sets, function(x) {length(x)})
  z <- 1:length(y)
  a <- unlist(mapply(function(x, i) {rep(x, y[[i]])}, weights, z))
  A <- diag(a)
  return(A)
}


#MFA as simple PCA
pca_func <- function(list, sets, ncomps) {
  #input is list of data tables
  new_sets <- shift_sets(sets)
  wghts <- weights(list)
  A <- weighting_matrix(wghts, sets)
  #combine data tables into one big matrix
  x_tilde <- matrix(unlist(list), nrow(list[[1]])) %*% A^(1/2)
  x_svd <- svd(x_tilde)
  eigen_values <- (sqrt(1/nrow(list[[1]])) * x_svd$d)^2
  if(!is.null(ncomps)) {
    eigen_values <- eigen_values[1:ncomps]
  }

  #calculate list of partial loadings
  negative_root_weights <- wghts ^ (-1/2)
  negative_root_A <- weighting_matrix(negative_root_weights, sets)
  q <- t(t(x_svd$v) %*% negative_root_A)
  partial_loadings <- list()
  for(i in 1:length(sets)) {
    loading <- t(subset(t(q), select = new_sets[[i]]))
    if(is.null(ncomps)) {
      partial_loadings[[i]] <- loading
    } else {
      partial_loadings[[i]] <- loading[ ,1:ncomps]
    }
  }

  #list of partial factor scores
  F_partial <- list()
  for(i in 1:length(list)) {
    partial_score <- length(list) * wghts[i] * list[[i]] %*% partial_loadings[[i]]
    if(is.null(ncomps)) {
      F_partial[[i]] <- partial_score
    } else {
      F_partial[[i]] <- partial_score[ ,1:ncomps]
    }

  }


  #save results we want in a list (we don't really need all of these but it is useful to have them while we test these functions against the example)
  out <- list(
    weightings_matrix = A,
    simple_pca = x_svd,
    eigen_values = eigen_values,
    loadings = q,
    partial_loadings = partial_loadings,
    partial_factor_scores = F_partial
  )
  out
}


#Get the observation names from the dataset
obs_names <- function(data){
  n_obs <- nrow(data)
  v <- vector()
  #if the data do not have row names (usually in the 1st column), we create;
  #if the data has row names(ie.the 1st column are characters),get it.
  if(is.numeric(data[,1])==TRUE){
    for (i in 1:n_obs){
      v <- c(v, paste("obs",i,sep = ""))
    }
  }else{
    v <- data[,1]
  }
  return(v)
}


#constructor function
make_mfa <- function(data,pca,ncomps) {
  if(is.null(ncomps)) {
    ncomps <- ncol(pca$loadings)
  }
  res <- list(
    eigen_values = pca$eigen_values, #vector
    singular_values = sqrt((pca$eigen_values)),
    common_factor_scores = (pca$simple_pca$u %*% diag(pca$simple_pca$d))[ ,1:ncomps], #matrix
    partial_factor_scores = pca$partial_factor_scores, #list
    loadings = pca$loadings[ ,1:ncomps],
    partial_loadings = pca$partial_loadings, #list
    observation_names = obs_names(data) #vector
  )
  class(res) <- "mfa"
  res
}





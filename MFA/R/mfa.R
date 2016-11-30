
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


#convert column names to column numbers
sets_numeric <- function(data, sets) {
  col_numbers <- lapply(sets, function(x) {match(x, colnames(data))})
  if(TRUE %in% is.na(col_numbers)) {
    stop("\n'sets' contains column name that is not in the data")
  }
  return(col_numbers)
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
  #test inputs
  test_data(data)
  test_sets_all(data, sets)
  if(FALSE %in% lapply(sets, is.numeric)) {
    sets <- sets_numeric(data, sets)
  }
  test_sets_numeric(data, sets)
  test_center_scale(center, sets)
  test_center_scale(scale, sets)

  new_sets <- shift_sets(sets)
  raw_tables <- lapply(sets, function(x) {subset(data, select = x)})
  if(is.logical(scale) && is.logical(center)) {
    tables <- lapply(raw_tables, function(x) {scale(x, center = center, scale = scale)})
  } else {
    if(is.logical(center)) {
      index <- 1:length(new_sets)
      tables <- mapply(function(x, i) {scale(x, center = center, scale = scale[new_sets[[i]]])}, raw_tables, index)
    } else {
      if(is.logical(scale)) {
        index <- 1:length(new_sets)
        tables <- mapply(function(x, i) {scale(x, center = center[new_sets[[i]]], scale = scale)}, raw_tables, index)
      } else {
        index <- 1:length(new_sets)
        tables <- mapply(function(x, i) {scale(x, center = center[new_sets[[i]]], scale = scale[new_sets[[i]]])}, raw_tables, index)
      }
    }
  }
  return(tables)
}


#determine the weights for each individual table
weights <- function(list) {
  test_tables(list)
  #create a nested list with the svd matrices for each data table
  svd_list <- lapply(list, svd)
  #create vector of weights
  wghts <- unlist(lapply(svd_list, function(x) {1/(x$d[1])^2}))
  return(wghts)
}


#create matrix of weightings
weighting_matrix <- function(weights, sets) {
  if(length(weights) != length(sets)) {
    stop("each data table must have a corresponding weight")
  }
  y <- lapply(sets, length)
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

  test_ncomp(ncomps, list)

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
  class(out) <- "pca"
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
make_mfa <- function(data, pca, ncomps) {
  test_pca(pca)
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




#test if input is a list of numeric or character vectors
test_sets_all <- function(data, sets) {
  if(!is.list(sets)) {
    stop("\n'sets' must be a list of numeric vectors")
  }
  if(length(sets) == 0) {
    stop("\n'sets' must have at least one entry")
  }
  if(length(unlist(sets)) > ncol(data)) {
    stop("number of columns in 'sets' cannot exceed total columns in data set")
  }
  if(length(unique(lapply(sets, is.numeric))) != 1 | length(unique(lapply(sets, is.character))) != 1) {
    stop("\n'sets' must be a list containing only numeric vectors or only character vectors")
  }
  if(length(unique(lapply(sets, is.vector))) != 1) {
    stop("\n'sets' must be a list of vectors")
  }
  TRUE
}

#test sets if it is a list of numeric vectors
test_sets_numeric <- function(data, sets) {
  if(sum(sapply(unlist(sets), function(x) {round(x) - x})) != 0) {
    stop("\n'sets' can only contain integer values")
  }
  if(min(unlist(sets)) < 1) {
    stop("\n'sets' can only contain numbers greater than 0")
  }
  if(max(unlist(sets)) > ncol(data)) {
    stop("number of columns in 'sets' cannot exceed total columns in data set")
  }
  TRUE
}

#test that data is a matrix or data frame
test_data <- function(data) {
  if(!is.matrix(data) && !is.data.frame(data)) {
    stop("data must be in the form of a matrix or data frame")
  }
  TRUE
}

#check center and scale are logical values or numeric vectors
test_center_scale <- function(arg, sets) {
  if(!is.logical(arg) && length(arg) != length(unlist(sets))) {
    stop("\n'center' and 'scale' must each be a logical value or a numeric vector whose length is the number of active variables")
  }
  if(!is.logical(arg) && !is.numeric(arg)) {
    stop("\n'center' and 'scale' must each be a logical value or a numeric vector")
  }
  TRUE
}


#test list of data tables
test_tables <- function(list) {
  if(!is.list(list)) {
    stop("argument should be a list")
  }
  if(!is.na(match(FALSE, lapply(list, is.matrix)))) {
    stop("All elements of list should be a matrix")
  }
  if(length(unique(lapply(list, nrow))) != 1) {
    stop("All matrices in the list should have the same number of rows")
  }
  TRUE
}

#check number of components
test_ncomp <- function(ncomps, list) {
  if(!is.null(ncomps) && !is.numeric(ncomps)) {
    stop("\n'ncomps' must be 'NULL' or a numeric value")
  }
  if(is.numeric(ncomps) && round(ncomps) != ncomps) {
    stop("\n'ncomps' must be an integer")
  }
  if(is.numeric(ncomps) && ncomps < 1) {
    stop("\n'ncomps' must be positive")
  }
  if(is.numeric(ncomps) && ncomps > nrow(list[[1]])) {
    stop("\n'ncomps' cannot exceed number of observations")
  }
  TRUE
}


#check pca input
test_pca <- function(pca) {
  if(class(pca) != "pca") {
    stop("input should be object of class 'pca'")
  }
  if(is.null(pca$eigen_values)) {
    stop("object is missing eigenvalues")
  }
  if(is.null(pca$simple_pca)) {
    stop("object is missing simple PCA of transformed data")
  }
  if(is.null(pca$simple_pca$u) | is.null(pca$simple_pca$d) | is.null(pca$simple_pca$v)) {
    stop("\n'simple_pca' should contain results of singular value decomposition")
  }
  if(is.null(pca$partial_factor_scores)) {
    stop("object is missing partial factor scores")
  }
  if(is.null(pca$loadings)) {
    stop("object is missing loadings matrix")
  }
  if(is.null(pca$partial_loadings)) {
    stop("object is missing partial loadings")
  }
  TRUE
}

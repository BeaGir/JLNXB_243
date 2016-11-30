#' @title Lg
#' @description Computes the Lg coefficient between two tables
#' @param table1, a matrix
#' @param table2, a matrix
#' @return the Lg coefficient (scalar)
#' @examples
#'
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' tables <- data_tables(wines, sets, TRUE, scaling_vec)
#'
#' Lgtable1_table2 <- Lg(tables[[1]], tables[[2]])
#' @export

Lg <- function(table1, table2){

  # extract matrices from the tables
  nrow1 <- nrow(table1)
  nrow2 <- nrow(table2)
  mat1 <- as.matrix(unlist(table1), nrow1)
  mat2 <- as.matrix(unlist(table2), nrow2)

  # outer products of each table
  cross_table1 <- tcrossprod(mat1)
  cross_table2 <- tcrossprod(mat2)

  # weights: inverse of the first singular value squared
  wghts <- weights(list(mat1,mat2))

  # Lg coefficient as described in page 8 of Abdi, Williams, and Valentin
  Lg <- sum(diag(cross_table1 %*% cross_table2)) * (wghts[1] * wghts[2])
  Lg
}

check_table <- function(table1,table2){
  if (!is.numeric(table1)|!is.numeric(table2)){
    stop("\n 'table1' and 'table2' must be numeric")
  }
  if (!(nrow(table1) == nrow(table2))){
    stop("\n 'table1' and 'table2' must have the same number of rows")
  }
  TRUE
}






#' @title Lg_table
#' @description Computes the Lg coefficients between two tables
#' @param dataset, a data-set with numeric variables
#' @param setsarg, a list of indices defining subsets of the dataset
#' @return the Lg coefficient matrix between all subsets of data
#' @examples
#'
#' # With the wines dataset
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' tables <- data_tables(wines, sets, TRUE, scaling_vec)
#' sets_shift <- list(1:6,7:12,13:18,24:29,39:44)
#' dataset1 <- as.data.frame(tables)
#' Lg_dataset <- Lg_table(dataset1, sets_shift)
#'
#' Simpler exampler
#' dataset1 <- data.frame(V1 = 1, V2 = 1:10, V3 = c(-1,1), V4 = runif(5), V5 = runif(5))
#' sets1 <- list(1:2,3:4)
#' Lg_dataset <- Lg_table(dataset1, sets1)
#' @export
Lg_table <- function(dataset, setsarg){
  # Inputs: A dataset with numeric variables and
  #       : A list of variables (correspodning to columns of dataset)
  # Output: The Lg coefficient matrix between the tables defined by the sets

  check_sets(dataset, setsarg)

  # Convert the dataset to a matrix
  data_matrix <- data.matrix(dataset)

  # Split the data matrix according to the sets
  tables <- lapply(setsarg, FUN = function(vec, data = data_matrix){data[,vec]})

  # mat[i,j] contains the Lg coefficnet between table i and table j
  # mat[i,i] = 1 for all i
  mat <- matrix(1, nrow = length(setsarg), ncol = length(setsarg))
  mat <- matrix(
    mapply(function(x,i,j){Lg(tables[[i]],tables[[j]])},
           mat, row(mat), col(mat)),
    nrow = nrow(mat))
  mat
}

check_sets <- function(dataset, sets){
  if (!(is.data.frame(dataset))){
    stop("\n 'dataset' must be a data-set")
  }

  if (!(is.list(sets))){
    stop("\n 'sets' must be a list")
  }
  for (i in 1:length(sets)){
    seti = sets[[i]]
    if (!is.numeric(seti)){
      stop("\n 'sets' must be a list of numeric indices")
    }
    if (min(seti) <=0 | max(seti) > ncol(dataset)){
      stop("\n 'sets' must be consistent with the dataset dimensions")
    }
    for (j in 1:length(seti)){
      if (!(is.numeric(unlist(dataset[,seti[j]])))){
        stop("\n 'sets' must designate numeric variables of the dataset")
      }
    }
  }
  TRUE
}



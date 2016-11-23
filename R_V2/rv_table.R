#' @title RV_table
#' @description Computes the Rv coefficient between two tables \code{"rv_table"}
#' @param dataset, a data-set with numeric variables
#' @param setsarg, a list of indices defining subsets of the data-set
#' @return the Rv coefficient matrix between all subsets of data
#' @export
#' @examples
#' 
#' # With the data-set wines
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' tables <- data_tables(wines, sets, TRUE, scaling_vec)
#' sets_shift <- list(1:6, 7:12, 13:18)
#' dataset1 <- data.frame(cbind(tables[[1]], tables[[2]], tables[[3]]))
#' 
#' RV_dataset <- RV_table(dataset1, sets_shift)
#' 
#' 
#' 
#' # More simple example
#' dataset1 <- data.frame(V1 = 1, V2 = 1:10, V3 = c(-1,1), V4 = runif(5), V5 = runif(5))
#' sets1 <- list(1:2, 3:4)
#' RV_dataset <- RV_table(dataset1, sets1)
#' 
#' 
#' 

source('rv.R')

RV_table <- function(dataset, setsarg = list(1:2, 3:4)){
  # INPUT
  # dataset : A dataset with numeric variables
  # setsarg : A list of variables (corresponding to columns of the dataset)
  # OUTPUT
  # data_matrix : The Rv coefficient matrix between the tables defined by the sets
  
  check_sets(dataset, setsarg)
  
  # We convert the dataset to a matrix
  data_matrix = data.matrix(dataset)
  
  
  # We split it according to the sets.
  tables = lapply(setsarg, FUN = function(vec,data = data_matrix){data[,vec]})
  
  # mat[i,j] contains the Rv coefficient between table i and table j
  # mat[i,i] = 1 for all i
  mat = matrix(1, nrow = length(setsarg), ncol = length(setsarg))
  
  mat = matrix(
    mapply(function(x, i, j){RV(tables[[i]],tables[[j]])}, 
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
    if (min(seti <=0)|max(seti)> ncol(dataset)){
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



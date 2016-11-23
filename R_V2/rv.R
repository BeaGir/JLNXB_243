#' @title RV
#' @description Computes the Rv coefficient between two tables \code{"rv"}
#' @param table1, a matrix
#' @param table2, a matrix with the same number of rows than table1
#' @return the Rv coefficient (scalar)
#' @export
#' @examples
#' 
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' tables <- data_tables(wines, sets, TRUE, scaling_vec)
#' 
#' RVtable1_table2 <- RV(tables[[1]], tables[[2]])
#' 

RV <- function(table1, table2){
  # INPUT
  # Two matrices (subsets of a dataframe)
  # OUTPUT
  # The Rv coefficient between these two tables
  
  check_table(table1, table2)
  
  # Cross-Products of the two tables
  cross_table1 = tcrossprod(table1)
  cross_table2 = tcrossprod(table2)
  
  trace_cross = sum(diag(cross_table1 %*% cross_table2))
  trace1 = sum(diag(cross_table1 %*% cross_table1))
  trace2 = sum(diag(cross_table2 %*% cross_table2))
  
  trace_cross / sqrt(trace1 * trace2)
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

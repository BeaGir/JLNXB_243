#' @title Lg
#' @description Computes the Lg coefficient between two tables
#' @param table1, a matrix
#' @param table2, a matrix
#' @return the Lg coefficient (scalar)
#' @export
#' @examples
#'
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' tables <- data_tables(wines, sets, TRUE, scaling_vec)
#'
#' Lgtable1_table2 <- Lg(tables[[1]], tables[[2]])
#'

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

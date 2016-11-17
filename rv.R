RV <- function(table1, table2){
  # INPUT
  # 2 matrices (subsets of a dataframe)
  
  # OUTPUT
  # The Rv coefficient between these two tables
  
  # Cross-Products of the two tables
  cross_table1 = tcrossprod(table1)
  cross_table2 = tcrossprod(table2)
  
  trace_cross = sum(diag(cross_table1 %*% cross_table2))
  trace1 = sum(diag(cross_table1 %*% cross_table1))
  trace2 = sum(diag(cross_table2 %*% cross_table2))
  
  trace_cross / sqrt(trace1 * trace2)
}

RV_table <- function(dataset, setsarg = list(1:6, 7:12, 13:17)){
  # INPUT
  # dataset : A dataset
  # setsarg : A list of variables (corresponding to columns of the dataset)
  
  # OUTPUT
  # data_matrix : The Rv coefficient matrix between the tables defined by the sets
  
  # We convert the dataset to a matrix
  data_matrix = data.matrix(dataset)
  
  # We verify that the sets called are consistent with the datasets size
  if(max(setsarg[[length(setsarg)]]) > ncol(data_matrix)){
    stop("Your data-set is not consistent with the given sets")
  }
  
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


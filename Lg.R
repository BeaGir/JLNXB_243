# Function : Lg
# Inputs   : 2 tables (subsets of a dataframe)
# Output   : The Lg coeffient between these two tables

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



# Function : Lg_table
# Inputs   : a dataset
#          : a list of indices of the dataset denoting different tables
# Output   : A matrix of Lg coefficients between the tables indicated

Lg_table <- function(dataset, sets = sets){

  # Convert the dataset to a matrix
  data_matrix <- data.matrix(dataset)

  # Split the data matrix according to the sets
  tables <- data_tables(data_matrix, sets, TRUE, TRUE)

  # mat[i,j] contains the Lg coefficnet between table i and table j
  # mat[i,i] = 1 for all i
  mat <- matrix(1, nrow = length(sets), ncol = length(sets))
  mat <- matrix(
          mapply(function(x,i,j){Lg(tables[i],tables[j])},
          mat, row(mat), col(mat)),
          nrow = nrow(mat))

  mat
}


# Examples

# get the data
wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
tables <- data_tables(wines, sets, TRUE, scaling_vec)
round(tables[[1]], 2) #compare this output to (54) in the paper

# Lg between two tables
Lg(tables[1],tables[2])
Lg(tables[4],tables[9])

# Matrix of Lg coefficients
# there is a warning message that NAs are introduced by coercion. I've chosen to ignore this
options(warn=-1)
Lg_table(wines, c(sets[1],sets[2],sets[3],sets[5],sets[8]))
Lg_table(wines, c(sets[4],sets[6],sets[9]))
Lg_table(wines, c(sets[7],sets[10]))


# Check to see if sets called are consistent with the dataset's size
if(max(sets[[length(sets)]]) > ncol(data_matrix)){
  stop("Your dataset is not consistent with the given sets")
}

#' @title bootstrap
#' @description method of bootstrap for mfa object
#' @param mfa the mfa object created by the mfa function
#' @param value the number of times of bootstrap
#' @return a list, containing the mean, standard deviation and bootstrap ratio; also, the plot of bootstrap ratio for each dimension is created
#' @examples
#'
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' mymfa1 <- mfa(wines, sets, ncomps = 2, T, scaling_vec)
#'
#' bootstrap(mymfa1,1000)
#' @export
bootstrap<- function(mfa,value) UseMethod("bootstrap")

#' @export
bootstrap.mfa <- function(mfa,value){
  check_value(value)

  f_list <- list()
  n_table <- length(mfa$partial_factor_scores)

  #sampling and get the list of bootstrapped factor scores from each sample
  for(i in 1:value){
    b <- sample(1:n_table, n_table,replace = TRUE)
    sum <- matrix(0,nrow(mfa$partial_factor_scores[[1]]),
                  ncol(mfa$partial_factor_scores[[1]]))
    for (j in b){
      sum = sum + mfa$partial_factor_scores[[j]]
    }
    f = sum/n_table
    f_list[[i]] <- f
  }


  #calculate the mean of bootstrap samples
  sample_mean <- Reduce("+",f_list)/value


  #calculate the sd of bootstrap samples
  difference_square <- list()
  for (i in 1:value){
    difference_square[[i]]<-(f_list[[i]]-sample_mean)*(f_list[[i]]-sample_mean)
  }
  sample_variance <- Reduce("+",difference_square)/value


  #calculate the bootstrap ratio
  t_star <- sample_mean/sqrt(sample_variance)

  #create output list
  output <- list(sample_mean, sqrt(sample_variance), t_star)
  names(output) <- c("mean", "sd", "bootstrap_ratio")



  #plotting the bootstrap ratio for the mfa object
  dimension <- ncol(mfa$partial_factor_scores[[1]])
  for(i in 1:dimension){
    par(xpd=F)
    barplot(output$bootstrap_ratio[,i],main = "Bootstrap Ratio Plot",
            xlab = paste("Dimension",i,sep = ""), ylab = "Bootstrap Ratio",
            names.arg = mfa$observation_names, cex.names= 0.8, horiz = TRUE)
    abline(v=0)
    abline(v=-3)
    abline(v=3)
  }

  return(output)
}




#private function to check the input bootstrap times
check_value <- function(value){
  if(length(value)!= 1){
    stop("\n'value'(the number of times of bootstrap) must have length 1")
  }
  if (value %% (as.integer(value)) != 0){
    stop("\n 'value'(the number of times of bootstrap) must be an integer")
  }
  if(value <= 0){
    stop("\n'value'(the number of times of bootstrap) must be positive")
  }
  TRUE
}

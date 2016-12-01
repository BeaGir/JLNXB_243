#' @title Plot of object mfa
#' @description Plots factor score, partial factor score and partial loadings
#' @param mfa an object of class \code{"mfa"}
#' @param type indicating which plot to output. 1: factor score plot; 2:factor score and partial loadings; 3: produce factor score and partial loadings on all components; 4: partial factor score; 5: partial loadings
#' @param d1 indicating the x-axis
#' @param d2 indicating the y-axis
#' @param X indicating the target table to produce a plot
#' @param loading_labels the labels for partial loadings
#' @param \dots arguments to be passed to/from other methods
#' @examples
#'
#' wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
#' sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
#' scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
#' mymfa <- mfa(wines, sets, ncomps = 2, T, scaling_vec)
#'
#' plot(mymfa, type = 1)
#' plot(mymfa, type = 2, X = 1)
#' plot(mymfa, type = 2, X = 1, loading_labels = c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral", "Smoky", "Citrus"))
#' plot(mymfa, type = 3, loading_labels = NULL)
#' plot(mymfa, type = 4, X = 1)
#' plot(mymfa, type = 5, X = 1)
#'
#' @export
plot.mfa <- function(mfa, type, d1 = 1, d2 = 2, X = 1, loading_labels = NULL, cex = 1, ...) {
  if (type != 1 & type != 2 & type != 3 & type != 4 & type != 5) {
    stop("invalid type input: pecify from 1, 2, 3, 4 or 5")
  }
  else {
    #type = 1, plot_factor_scores
    if (type == 1) {
      plot_factor_scores(mymfa = mfa, d1 = d1, d2 = d2, cex = cex)
    }
    #type = 2, plot_pfs_vl
    else if (type ==2 ) {
      plot_pfs_vl(mymfa = mfa, X=X, d1 = d1, d2 = d2, loading_labels = loading_labels,  cex = cex)
    }
    #type = 4, plot_pfs
    else if (type ==4 ) {
      plot_pfs(mymfa = mfa, X=X, d1 = d1, d2 = d2,  cex = cex)
    }
    #type = 5, plot_vl
    else if (type ==5 ) {
      plot_vl(mymfa = mfa, X=X, d1 = d1, d2 = d2, loading_labels = loading_labels , cex = cex)
    }
    #type = 3, plot_pfs_vl_all
    else {
      plot_pfs_vl_all(mymfa=mfa, d1 = d1, d2 = d2, loading_labels = loading_labels, cex = cex)
    }
  }
}

#all below are auxiliary functions for plot.mfa() method
#plot for factor scores
#mymfa: the mfa object
#d1: horizontal axes
#d2: vertical axes
plot_factor_scores <- function(mymfa, d1 = 1, d2 = 2,  cex = 1) {
  if (ncol(mymfa$common_factor_scores) < max(d1, d2)) {
    stop("invalid dimension input: common factor score does not have enough dimension")
  }
  else {
    #generated random color for each data point
    data <- data.frame(x = mymfa$common_factor_scores, objects = mymfa$observation_names, cl = rainbow(nrow(mymfa$common_factor_scores)))
    margin <- max(max(abs(data[,d1])),max(abs(data[,d2])))
    plot(data[, d1], data[, d2], col= data$cl, pch=16, axes = FALSE,
         panel.first = grid(),
         xlim = c(-1*margin-0.5,margin+0.5),
         ylim = c(-1*margin-0.5,margin+0.5),
         xlab = NA,
         ylab = NA, main = "Factor Scores", cex = cex, cex.main = cex*1.5)
    legend("bottomleft", cex=0.7 *cex ,legend = data$objects ,col=data$cl ,pch=16)
    arrows(x0 = -1*margin-0.2, y0 = 0, x1 = margin+0.2, y1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    arrows(y0 = -1*margin-0.2, x0 = 0, y1 = margin+0.2, x1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    d1_string <- paste(d1)
    d2_string <- paste(d2)
    text(0, margin+0.3, d2_string, cex= cex)
    text(margin+0.3, 0, d1_string, cex= cex)
  }
}

#plot for partial factor scores
plot_pfs <- function (mymfa, X=1, d1 = 1, d2 = 2, cex = 1) {
  if (ncol(mymfa$common_factor_scores) < max(d1, d2)) {
    stop("invalid dimension input: partial factor score does not have enough dimension")
  }
  else {
    data <- data.frame(mymfa$partial_factor_scores[[X]],objects = mymfa$observation_names, cl = rainbow(nrow(mymfa$common_factor_scores)))
    
    margin <- max(max(abs(data[,d1])),max(abs(data[,d2])))
    plot(data[, d1], data[, d2], col= data$cl, pch=16, axes = FALSE,
         panel.first = grid(),
         xlim = c(-1*margin-0.5,margin+0.5),
         ylim = c(-1*margin-0.5,margin+0.5),
         xlab = NA,
         ylab = NA,
         main ="Partial Factor Scores" , cex = cex, cex.main = 1.5 * cex)
    legend("bottomleft", cex= 0.7 * cex ,legend = data$objects ,col=data$cl ,pch=16)
    arrows(x0 = -1*margin-0.2, y0 = 0, x1 = margin+0.2, y1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    arrows(y0 = -1*margin-0.2, x0 = 0, y1 = margin+0.2, x1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    d1_string <- paste(d1)
    d2_string <- paste(d2)
    text(0, margin+0.3, d2_string, cex= cex)
    text(margin+0.3, 0, d1_string, cex= cex)
    
  }
}



#plot for variable loadings
plot_vl <- function (mymfa, X=1, d1 = 1, d2 = 2, loading_labels = NULL, cex = 1) {
  if (ncol(mymfa$common_factor_scores) < max(d1, d2)) {
    stop("invalid dimension input: variable loading do not have enough dimension")
  }
  else {
    
    loadingdata <- data.frame(mymfa$partial_loadings[[X]] )
    #talked to Gaston, he said the rescale in the paper has some issues
    #Gaston confirmed that we can rescale using the factors as we want
    rescaled_loadings <- data.frame(x = 0.8*loadingdata[,d1]/sd(loadingdata[,d1]), y = 0.4*loadingdata[,d2]/sd(loadingdata[,d2]))
    if(is.null(loading_labels)){
      v <- NULL
      for (i in 1:nrow(loadingdata)){
        v <- c(v, paste("loading",i,sep = ""))
      }
    }else{
      v <- loading_labels
    }
    data <- data.frame(rescaled_loadings, objects = v, cl = rainbow(length(v)))
    
    margin <- max(max(abs(data[,1])),max(abs(data[,2])))
    plot(data[, 1], data[, 2], col= data$cl, pch=16, axes = FALSE,
         panel.first = grid(),
         xlim = c(-1*margin-0.5,margin+0.5),
         ylim = c(-1*margin-0.5,margin+0.5),
         xlab = NA,
         ylab = NA,
         main = "Variable Loadings" , cex = cex, cex.main = 1.5 * cex)
    if (!is.null(loading_labels)) {
      #decided to let user input data labels themselves since it is not included in csv file
      text(rescaled_loadings[,d1], rescaled_loadings[,d2], labels=loading_labels[1:nrow(rescaled_loadings)], cex= 0.7*cex, pos=4)
    }
    else {
      legend("bottomleft", cex= 0.7 * cex,legend = data$objects ,col=data$cl ,pch=16)
    }
    arrows(x0 = -1*margin-0.2, y0 = 0, x1 = margin+0.2, y1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    arrows(y0 = -1*margin-0.2, x0 = 0, y1 = margin+0.2, x1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    d1_string <- paste(d1)
    d2_string <- paste(d2)
    text(0, margin+0.3, d2_string, cex= cex)
    text(margin+0.3, 0, d1_string, cex= cex)
    
  }
}

#plot for partial factor scores and variable loadings
#mymfa: the mfa object
#d1: horizontal axes
#d2: vertical axes
#X: which data table you would like to plot for
#loading_labels: the label that user could input themselves
plot_pfs_vl <- function (mymfa, X=1, d1 = 1, d2 = 2, loading_labels = NULL, cex = 1) {
  if (ncol(mymfa$common_factor_scores) < max(d1, d2)) {
    stop("invalid dimension input: factor score  and loadings do not have enough dimension")
  }
  else {
    data <- data.frame(mymfa$partial_factor_scores[[X]],objects = mymfa$observation_names, cl = rainbow(nrow(mymfa$common_factor_scores)))
    
    loadingdata <- data.frame(mymfa$partial_loadings[[X]] )
    #talked to Gaston, he said the rescale in the paper has some issues
    #Gaston confirmed that we can rescale using the factors as we want
    rescaled_loadings <- data.frame(x = 0.8*loadingdata[,d1]/sd(loadingdata[,d1]), y = 0.4*loadingdata[,d2]/sd(loadingdata[,d2]))
    
    margin <- max(max(abs(data[,d1])),max(abs(data[,d2])), max(abs(rescaled_loadings[,d1])), max(abs(rescaled_loadings[,d2])))
    plot(data[, d1], data[, d2], col= data$cl, pch=16, axes = FALSE,
         panel.first = grid(),
         xlim = c(-1*margin-0.5,margin+0.5),
         ylim = c(-1*margin-0.5,margin+0.5),
         xlab = NA,
         ylab = NA,
         main = "Partial Factor Score \nand Variable Loadings" , cex = cex, cex.main = 1 * cex)
    legend("bottomleft", cex= 0.7 * cex ,legend = data$objects ,col=data$cl ,pch=16)
    arrows(x0 = -1*margin-0.2, y0 = 0, x1 = margin+0.2, y1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    arrows(y0 = -1*margin-0.2, x0 = 0, y1 = margin+0.2, x1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    d1_string <- paste(d1)
    d2_string <- paste(d2)
    text(0, margin+0.3, d2_string, cex= cex)
    text(margin+0.3, 0, d1_string, cex= cex)
    
    points(rescaled_loadings[,d1], rescaled_loadings[,d2], col = "grey", pch=12, cex = cex)
    if (!is.null(loading_labels)) {
      #decided to let user input data labels themselves since it is not included in csv file
      text(rescaled_loadings[,d1], rescaled_loadings[,d2], labels=loading_labels[1:nrow(rescaled_loadings)], cex= 0.7*cex, pos=4)
    }
  }
}

#printing all 10 plots
#mymfa: the mfa object
#d1: horizontal axes
#d2: vertical axes
#loading_labels: the label that user could input themselves
plot_pfs_vl_all <- function (mymfa, d1 = 1, d2 = 2, loading_labels = NULL, cex = 1) {
  total <- length(mymfa$partial_factor_scores)
  if (is.null(loading_labels)) {
    #the reason I didn't output all plots into one page
    #is because that we are developing a package that could do MFA for any eligible data
    #while plotting for other eligible data, we are not sure totally how many plots it needs to produce
    #in this case, producing all plots on one single sheet is very risky
    #since we are not sure how many plots we totally could have
    #I talked to Gaston and he agreed with me that we'd better not produce all plots on one page
    #thus I am plotting them out one by one in new windows
    for (i in 1:total) {
      #dev.new()
      plot_pfs_vl(mymfa, i, d1, d2, loading_labels, cex = cex)
    }
  }
  else {
    for (i in 1:total) {
      #dev.new()
      plot_pfs_vl(mymfa, i, d1, d2, loading_labels = loading_labels[,i], cex = cex)
    }
  }
}


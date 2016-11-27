
#plot for factor scores
#mymfa: the mfa object
#d1: horizontal axes
#d2: vertical axes
plot_factor_scores <- function(mymfa, d1 = 1, d2 = 2) {
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
         ylab = NA)
    legend("bottomleft", cex=0.5,legend = data$objects ,col=data$cl ,pch=16)
    arrows(x0 = -1*margin-0.2, y0 = 0, x1 = margin+0.2, y1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    arrows(y0 = -1*margin-0.2, x0 = 0, y1 = margin+0.2, x1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    d1_string <- paste(d1)
    d2_string <- paste(d2)
    text(0, margin+0.3, d2_string, cex= 0.7)
    text(margin+0.3, 0, d1_string, cex= 0.7)
  }
}

#plot for partial factor scores  
  plot_pfs <- function (mymfa, X=1, d1 = 1, d2 = 2) {
    if (ncol(mymfa$common_factor_scores) < max(d1, d2)) {
      stop("invalid dimension input: common factor score does not have enough dimension")
    }
    else {
      data <- data.frame(mymfa$partial_factor_scores[[X]],objects = mymfa$observation_names, cl = rainbow(nrow(mymfa$common_factor_scores)))
     
      margin <- max(max(abs(data[,d1])),max(abs(data[,d2])))
      plot(data[, d1], data[, d2], col= data$cl, pch=16, axes = FALSE, 
           panel.first = grid(), 
           xlim = c(-1*margin-0.5,margin+0.5), 
           ylim = c(-1*margin-0.5,margin+0.5),
           xlab = NA,
           ylab = NA)
      legend("bottomleft", cex=0.5,legend = data$objects ,col=data$cl ,pch=16)
      arrows(x0 = -1*margin-0.2, y0 = 0, x1 = margin+0.2, y1 = 0, length=0.05,angle=20,
             code = 2, lwd = 2)
      arrows(y0 = -1*margin-0.2, x0 = 0, y1 = margin+0.2, x1 = 0, length=0.05,angle=20,
             code = 2, lwd = 2)
      d1_string <- paste(d1)
      d2_string <- paste(d2)
      text(0, margin+0.3, d2_string, cex= 0.7)
      text(margin+0.3, 0, d1_string, cex= 0.7)
      
     }
    }



#plot for variable loadings
plot_vl <- function (mymfa, X=1, d1 = 1, d2 = 2, loading_labels = NULL) {
  if (ncol(mymfa$common_factor_scores) < max(d1, d2)) {
    stop("invalid dimension input: common factor score does not have enough dimension")
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
         ylab = NA)
    if (!is.null(loading_labels)) {
      #decided to let user input data labels themselves since it is not included in csv file
      text(rescaled_loadings[,d1], rescaled_loadings[,d2], labels=loading_labels[1:nrow(rescaled_loadings)], cex= 0.7, pos=4)
    }
    else {
    legend("bottomleft", cex=0.5,legend = data$objects ,col=data$cl ,pch=16)
      }
    arrows(x0 = -1*margin-0.2, y0 = 0, x1 = margin+0.2, y1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    arrows(y0 = -1*margin-0.2, x0 = 0, y1 = margin+0.2, x1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    d1_string <- paste(d1)
    d2_string <- paste(d2)
    text(0, margin+0.3, d2_string, cex= 0.7)
    text(margin+0.3, 0, d1_string, cex= 0.7)
    
  }
}

#plot for partial factor scores and variable loadings
#mymfa: the mfa object
#d1: horizontal axes
#d2: vertical axes
#X: which data table you would like to plot for
#loading_labels: the label that user could input themselves
plot_pfs_vl <- function (mymfa, X=1, d1 = 1, d2 = 2, loading_labels = NULL) {
  if (ncol(mymfa$common_factor_scores) < max(d1, d2)) {
    stop("invalid dimension input: common factor score does not have enough dimension")
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
         ylab = NA)
    legend("bottomleft", cex=0.5,legend = data$objects ,col=data$cl ,pch=16)
    arrows(x0 = -1*margin-0.2, y0 = 0, x1 = margin+0.2, y1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    arrows(y0 = -1*margin-0.2, x0 = 0, y1 = margin+0.2, x1 = 0, length=0.05,angle=20,
           code = 2, lwd = 2)
    d1_string <- paste(d1)
    d2_string <- paste(d2)
    text(0, margin+0.3, d2_string, cex= 0.7)
    text(margin+0.3, 0, d1_string, cex= 0.7)
    
    points(rescaled_loadings[,d1], rescaled_loadings[,d2], col = "grey", pch=12)
    if (!is.null(loading_labels)) {
      #decided to let user input data labels themselves since it is not included in csv file
      text(rescaled_loadings[,d1], rescaled_loadings[,d2], labels=loading_labels[1:nrow(rescaled_loadings)], cex= 0.7, pos=4)
    }
  }
}

#printing all 10 plots
#mymfa: the mfa object
#d1: horizontal axes
#d2: vertical axes
#loading_labels: the label that user could input themselves
plot_pfs_vl_all <- function (mymfa, d1 = 1, d2 = 2, loading_labels = NULL) {
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
      plot_pfs_vl(mymfa, i, d1, d2, loading_labels)
    }
  }
  else {
    for (i in 1:total) {
      #dev.new()
      plot_pfs_vl(mymfa, i, d1, d2, loading_labels = loading_labels[,i])
    }
    
  }
}

#make it a method

plot.mfa <- function(mfa, type, d1 = 1, d2 = 2, X = 1, loading_labels = NULL, ...) {
  if (type != 1 & type != 2 & type != 3 & type != 4 & type != 5) {
    stop("invalid type input: pecify from 1, 2, 3, 4 or 5")
  }
  else {
    #type = 1, plot_factor_scores
    if (type == 1) {
      plot_factor_scores(mymfa = mfa, d1 = d1, d2 = d2)
    }
    #type = 2, plot_pfs_vl
    else if (type ==2 ) {
      plot_pfs_vl(mymfa = mfa, X=X, d1 = d1, d2 = d2, loading_labels = loading_labels)
    }  
    #type = 4, plot_pfs
    else if (type ==4 ) {
      plot_pfs(mymfa = mfa, X=X, d1 = d1, d2 = d2)
    }  
    #type = 5, plot_vl
    else if (type ==5 ) {
      plot_vl(mymfa = mfa, X=X, d1 = d1, d2 = d2, loading_labels = loading_labels)
    }  
    #type = 3, plot_pfs_vl_all
    else {
      plot_pfs_vl_all(mymfa=mfa, d1 = d1, d2 = d2, loading_labels = loading_labels)
    }
  }
}


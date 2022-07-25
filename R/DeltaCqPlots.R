#' plots delta Cq values as box-plots
#'
#' delta.Cq data frame has to be created before
#'
#' It is possible to use the regression model to display a prediction axis.
#' It is also possible to plot the P Values for a t.test of the boxplots.
#'
#' All these extra plot options can be manually done after returning the ggplot object for more customisation!
#'
#' @import ggplot2 ggpubr
#' @param points Show individual datapoints? std TRUE
#' @param useModel Use the regression model for predicition.
#' @param predictionRange this vector will be used for the prediction. corresponds to the deltaCq y axis!
#' @param reTransform If the linear transformation (linSqrtTransform) was used set to TRUE
#' @param pVal Plots the P value from stat_compare_means (ggpubr) to the plot
#' @param comparisons a list of 2 item vectors to define which P values are presented.
#' @return returns a ggplot object.
#' @export
plot.delta.Cq.box <- function(points = TRUE, useModel = FALSE, predictionRange = seq(-8,8,2), reTransform = FALSE, pVal = FALSE, comparisons = list(c(1, 2))){

  if(!exists("delta.Cq")){ # Check if delta.Cq is available
    stop("No delta.cq list available - please run delta.Cq.data()")
  }

  # Get values for the second axis. (prediction from the model in global space)
  if (useModel){
    if (!exists("model.delta.Cq")){
      return("Model not present, but useModel set TRUE")
    } else {
      scales.labels <- c()
      for (val in predictionRange) {
        scales.labels <- c(scales.labels, invest(model.delta.Cq, val, interval = "inversion", mean.response = TRUE, lower = -200, upper = 200)$estimate)
      }
    }
    if (reTransform){
      scales.labels <- round(reTransform(scales.labels),1)
    }
  }

  p <- ggplot(delta.Cq, aes(x = factor(sample), y = delta.Cq)) +
    labs(x="", y = bquote(Delta*"Cq"~"(Cq" ["offtarget"] - "Cq" ["target"]~")")) +
    stat_boxplot(geom ='errorbar', width = .1, position = position_dodge(width = 0.3), size = 0.2) +
    geom_boxplot(outlier.shape = 4, outlier.size = .3, outlier.colour = "grey",width= 0.4, varwidth = F, position = position_dodge(width = 0.3), fatten = 1)

  if(points){
    p <- p +
      geom_jitter(col = "red", pch = 4, width = 0.2, alpha = 0.5)
  }

  if (useModel) {
    p  <- p + scale_y_continuous(
      sec.axis = sec_axis(
        ~ . ,
        name = "model prediction [%]",
        breaks = predictionRange,
        labels = c(scales.labels)
      ),
      breaks = predictionRange
    )
  }

  if (pVal) {
    p <- p + stat_compare_means(method = "t.test", comparisons = comparisons)
  }
  #

  return(p)
}


#' plots delta Cq values against the concentration
#'
#' will use the data.Cq list as input.
#' Only numeric values will be used (100 = 100 % = "100")
#'
#' @param CqType list of Cq types that should be plotted.
#' @param linSqrtTrans Will transform the values to linearise the values! this is basically a shifted square root representation.
#' @param method to generate the delta Cq values. See delta.Cq.data()
#' @param xlab x axis title for the plots.
#' @return returns a R base plot.
#' @export
plot.delta.Cq.overview <- function(CqType = c("SD","TP"), linSqrtTrans = FALSE, method = "c", xlab = "mixture [%]"){

  if(!exists("data.Cq")){ # Check if delta.Cq is available
    stop("No data.cq list available - please run make.Cq.data()")
  }

  # data frame of all Cq Diffs
  data <- data.frame(sample = numeric(), delta.Cq = numeric(), CqType = character())

  # get all the delta Cq values for Cq types.
  for (type in CqType) {
    if(eval(parse(text = paste0("is.null(data.Cq[[1]]$", type,")")))){
      stop("oh, wrong Cq type?")
    }
    thisData <- delta.Cq.data(CqType = type, return = TRUE, onlyNumeric = TRUE, method = method)
    #To be sure that it is really numeric:
    #thisData$sample <- as.numeric(thisData$sample)

    ## Shifted square root transformation:
    if (linSqrtTrans) {
      thisData <- linSqrtTransform(thisData)
    }

    thisData <- cbind(thisData, CqType = type)
    data <- rbind(data, thisData)
  }

  # Parameters for Plotting
  l <- length(CqType)
  yl <- c(round(min(data$delta.Cq)),round(max(data$delta.Cq)))

  # Plot in a grid:
  if (l == 1) {

  } else if (l <= 3) {
    par(mfrow=c(1,3))
  } else if (l <= 6) {
    par(mfrow=c(2,3))
  } else {
    par(mfrow=c(3,3))
  }

  for (type.n in CqType) {
    plot(x = data$sample[which(data$CqType == type.n)],
         y = data$delta.Cq[which(data$CqType == type.n)],
         pch = 16,
         main = paste("Datapoints - Cq value by", type.n),
         ylab = "Cq difference",
         xlab = xlab,
         ylim = yl)
  }
}

#' plots delta Cq values against the square root of the concentration
#'
#' will use the data.Cq list as input.
#' Only numeric values will be used (100 = 100 % = "100")
#'
#' @param CqType list of Cq types that should be plotted.
#' @param method to generate the delta Cq values. See delta.Cq.data()
#' @param xlab x axis title for the plots.
#' @return returns a R base plot.
#' @export
plot.delta.Cq.differences <- function(CqType = c("SD","TP"), method = "c", xlab = "mixture [%] target of interest"){

}

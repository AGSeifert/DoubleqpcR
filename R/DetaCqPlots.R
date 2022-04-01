#' plots delta Cq values as box-plots
#'
#' delta.Cq data frame has to be created before
#'
#' @import ggplot2
#' @param points Show individual datapoints? std TRUE
#' @return returns a ggplot object.
#' @export
plot.delta.Cq.box <- function(points = TRUE){

  if(!exists("delta.Cq")){ # Check if delta.Cq is available
    stop("No delta.cq list available - please run delta.Cq.data()")
  }

  p <- ggplot(delta.Cq, aes(x = factor(sample), y = delta.Cq)) +
    labs(x="", y = bquote(Delta*"Cq"~"(Cq" ["offtarget"] - "Cq" ["target"]~")")) +
    stat_boxplot(geom ='errorbar', width = .1, position = position_dodge(width = 0.3), size = 0.2) +
    geom_boxplot(outlier.shape = 4, outlier.size = .3, outlier.colour = "grey",width= 0.4, varwidth = F, position = position_dodge(width = 0.3), fatten = 1)

  if(points){
    p <- p +
      geom_jitter(col = "red", pch = 4, width = 0.2, alpha = 0.5)
  }

  return(p)
}


#' plots delta Cq values against the concentration
#'
#' will use the data.Cq list as input.
#' Only numeric values will be used (100 = 100 % = "100")
#'
#' @param CqType list of Cq types that should be plotted.
#' @param method to generate the delta Cq values. See delta.Cq.data()
#' @param xlab x axis title for the plots.
#' @return returns a R base plot.
#' @export
plot.delta.Cq.overview <- function(CqType = c("SD","TP"), method = "c", xlab = "mixture [%]"){

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

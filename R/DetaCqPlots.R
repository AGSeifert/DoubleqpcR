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

  p <- ggplot(delta.Cq, aes(x = sample, y = delta.Cq)) +
    labs(x="", y = bquote(Delta*"Cq"~"(Cq" ["offtarget"] - "Cq" ["target"]~")")) +
    stat_boxplot(geom ='errorbar', width = .1, position = position_dodge(width = 0.3), size = 0.2) +
    geom_boxplot(outlier.shape = 4, outlier.size = .3, outlier.colour = "grey",width= 0.4, varwidth = F, position = position_dodge(width = 0.3), fatten = 1)

  if(points){
    p <- p +
      geom_jitter(col = "red", pch = 4, width = 0.2, alpha = 0.5)
  }

  return(p)
}

#' plots Cq values with the both genotypes on x and y axis
#'
#' This function will give you a Cq plot. Therefore the Cq values are plotted.
#' You need to have data.cq list from make.cq.data() function.
#'
#' @param CqType this is the Cq value type that should be used.
#' @param onlyNumeric Will only use samples that are a numerical (e.g. percentage)
#' @return returns a ggplot with the Cq values for both genotypes.
#' @export
plot.Cq <- function(CqType = "SD", onlyNumeric = FALSE){

  df <- Cq.data.mean(CqType = CqType, onlyNumeric = onlyNumeric, return = TRUE)

  plot <- ggplot(df, aes(x = mean.Cq.target, y = mean.Cq.offtarget, col = sample)) +
    geom_abline(intercept = 8, slope = 1, lty = "aa", color = "grey", size = .1) +
    geom_abline(intercept = 7, slope = 1, lty = "aa", color = "grey", size = .1) +
    geom_abline(intercept = 6, slope = 1, lty = "aa", color = "grey", size = .1) +
    geom_abline(intercept = 5, slope = 1, lty = "aa", color = "grey", size = .1) +
    geom_abline(intercept = 4, slope = 1, lty = "aa", color = "grey", size = .1) +
    geom_abline(intercept = 3, slope = 1, lty = "aa", color = "grey", size = .1) +
    geom_abline(intercept = 2, slope = 1, lty = "aa", color = "grey", size = .1) +
    geom_abline(intercept = 1, slope = 1, lty = "aa", color = "grey", size = .1) +
    geom_abline(intercept = 0, slope = 1, lty = "aa", color = "green", size = .1) +
    geom_abline(intercept = -1, slope = 1, lty = "aa", color = "grey", size = .1) +
    geom_abline(intercept = -2, slope = 1, lty = "aa", color = "grey", size = .1) +
    geom_abline(intercept = -3, slope = 1, lty = "aa", color = "grey", size = .1) +
    geom_abline(intercept = -4, slope = 1, lty = "aa", color = "grey", size = .1) +
    geom_abline(intercept = -5, slope = 1, lty = "aa", color = "grey", size = .1) +
    #geom_text(x = 5, y=5, label = "Delta Cq = 0", inherit.aes = F, show.legend = F, color = "green", size = 3, angle = 19) +
    geom_errorbar(aes(x = mean.Cq.target, y = mean.Cq.offtarget, xmin = mean.Cq.target - sd.Cq.target, xmax = mean.Cq.target + sd.Cq.target), lty = 1, size = .2, width = .3, show.legend = FALSE, inherit.aes = FALSE) +
    geom_errorbar(aes(x = mean.Cq.target, y = mean.Cq.offtarget, ymin = mean.Cq.offtarget - sd.Cq.offtarget, ymax = mean.Cq.offtarget + sd.Cq.offtarget),  lty = 1, size = .2, width = .1, show.legend = FALSE, inherit.aes = FALSE) +
    geom_point(aes(color = sample), cex = 5) +
    theme_classic() +
    labs(x = "Cq value (target allele primer)",
         y = "Cq value (offtarget allele primer)") +
    theme(legend.position=c(0.8,0.7), legend.text = element_text(colour="black", size=8, face="bold"))

    if(onlyNumeric){
       plot + scale_color_continuous(type = "viridis", name = "Mixture [%] target allel")
    } else{
      plot
    }


}

#' plots Cq values of each genotype againgst the pseudo log concentration.
#'
#' This function will give you a classic efficiency plot. Therefore the Cq values are plotted.
#' You need to have data.cq list from make.cq.data() function.
#'
#' ! The samplename is the concentration / mixture for the target genotype!
#' Only valid numeric samples will be used. Sample names like 100, "100", "100%" will be converted to 100. But other sample names like "Test" will be ignored.
#'
#' @param CqType this is the Cq value type that should be used.
#' @return returns a ggplot with the Cq values for both genotypes.
#' @export
plot.Cq.efficiency <- function(CqType = "SD"){

  df <- Cq.data.mean(CqType = CqType, onlyNumeric = TRUE, return = TRUE)


  ggplot(df) +
    geom_point(cex=3, aes(x = sample, y = mean.Cq.target, col = "target"))+
    geom_point(cex=3, aes(x = 100-sample, y = mean.Cq.offtarget, col = "offtarget"))+
    xlab(label = "Mixture [%] (pseudo_log scale)")+
    ylab(label = paste0("Cq value: ", CqType))+
    scale_x_continuous(trans = "pseudo_log")
}

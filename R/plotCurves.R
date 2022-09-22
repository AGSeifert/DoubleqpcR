#' plot fluorescence curves
#'
#' @import ggplot2
#' @param sample specify the sample from input.cq std: "all" will plot all curves. Can be Numeric e.g. 100 for dilution series etc. Can be Character e.g. "100" for a normal experiment.
#' @param target the target genotype "genotype A".
#' @param color.target The color for the target genotype curve (only if not all curves are plotted)
#' @param color.offtarget The color for the other genotype curve (only if not all curves are plotted)
#' @return a ggplot object
#' @export
plot.curve <- function(sample = "all", target = "Genotype A", color.target = "firebrick2", color.offtarget = "cornflowerblue"){
  if(!is.na(sample)){
  if(sample == "all"){
    plot <- ggplot(input.raw.melt.usedOnly, aes(x = Cycle, y = value, col = variable)) +
      geom_point(size=1, shape=20) +
      geom_line() +
      labs(x = "Cycle number", y = "Fluorescence", title = "Amplification Curves", subtitle = paste("Experiment:", sample)) +
      xlim(0,max(input.raw.melt.usedOnly$Cycle)) +
      ylim(min(input.raw.melt.usedOnly$value),pretty(max(input.raw.melt.usedOnly$value))[2]) +
      scale_color_grey(start=0.8, end=0.2) +
      theme_light() +
      theme(legend.position = "none")
  } else {   # Here numeric / String Difference?

    if (!any(input.cq$sample == sample)){
      stop("No such sample present in the data.")
    }


    # initialize Genotypes
    if(!any(unique(input.cq$type) == target)){
      stop("Target genotype not present!")
    }
    if(!length(unique(input.cq$type)) == 2){
      stop("More then two genotypes present. Check spelling?")
    }
    offtarget <- "Genotype B"
    for(genotype in unique(input.cq$type)) {
      if (genotype != target) {
        offtarget <- genotype
      }
    }

    # combines the wells, so that target is always first
    containing <- c(as.character(input.cq$well[input.cq$sample == sample & input.cq$type == target]), as.character(input.cq$well[input.cq$sample == sample & input.cq$type == offtarget]))
    containing.label <- containing
    annotation.label <- c(1,length(containing)) #/2+1
    containing.label[annotation.label[1]] <- paste(containing.label[annotation.label[1]], "-", target)
    containing.label[annotation.label[2]] <- paste(containing.label[annotation.label[2]], "-", offtarget)

    containing.A <- length(input.cq$well[input.cq$sample == sample & input.cq$type == target])
    containing.B <- length(input.cq$well[input.cq$sample == sample & input.cq$type == offtarget])

    if(is.numeric(sample)){
      subt <- paste0("Experiment: ", sample, "% ", target , " : ", 100-as.numeric(sample), "% ", offtarget)
    } else {
      subt <- paste0("Experiment: ", sample)
    }


    plot <- ggplot(input.raw.melt.usedOnly[input.raw.melt.usedOnly$variable %in% containing,], aes(Cycle, value, col=variable)) +
      geom_point(size=1, shape=20) +
      geom_line() +
      labs(x = "Cycle number", y = "Fluorescence", title = "Amplification Curves", subtitle = subt) +
      xlim(0,max(input.raw.melt.usedOnly$Cycle)) +
      ylim(min(input.raw.melt.usedOnly$value),pretty(max(input.raw.melt.usedOnly$value))[2]) +
      theme_light() +
      theme(legend.position = c(0.15,0.65), legend.key.size = unit(0, 'lines'), legend.background = element_rect(fill = "transparent", colour = NA)) +
      scale_color_manual("Experiment well", breaks = containing, values = c(rep(color.target,containing.A), rep(color.offtarget,containing.B)), labels=containing.label)

  }

  return(plot)

  } else {
    stop("NA sample can not be plotted.")
  }
}

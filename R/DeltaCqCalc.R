#' shows Cq values from a sample and the mean Delta Cq
#'
#' This function will show the Cq values from the samples.
#' You need to have input.cq "read.cqTable" or build it manually with Cq values present.
#'
#'
#' @import kableExtra
#' @import DT
#' @param sample specify the sample from input.cq
#' @param target the target genotype "genotype A".
#' @param CqType wich Cq values should be used. This can be a vector!
#' @param outliers logical if outliers are to be deleted from the output
#' @param outliers.method If a "Dixon" or "Grubbs" test should be used.
#' @param alpha alpha for outlier testing (0.05 = 95% significance)
#' @param outlier.range For Grubbs: input ignored, set to 6. For Dixon: This is only important for samples with 3 or less values. In this case the range of data (e.g. Range c(1,1.4,1.3) = 0.4) need to be at least outlier.range if an outlier test shoud happen. Normally outlier test for 3 or less values is not recommended. But this helps to get rid of clear outliers e.g. (2,2,30). My advice is to check the data also manually.
#' @param decimals decimals for the resulting table (ignored for format = "data")
#' @param format How the table will be formated. possible are "kable" and "DT" (work in progress) or "data" for pure dataframe, "fulldata" for a dataframe with difference, mean, and sd. Or any other input will give just the table.
#' @param silent should status be printed? (mostly for outlier detection)
#' @return returns a table with the delta Cq values with mean and standard deviation.
#' @export
table.Cq <- function(sample = NA, target = "Genotype A", CqType = c("TP","SD"), outliers = TRUE, outliers.method = "Grubbs", alpha = 0.05, outlier.range = 3, decimals = 3, format = "kable", silent = FALSE){
  # Control the input:
  if(is.na(sample)){
    stop("Not a valid sample.")
  }
  for (type in CqType) {
    if(!any(names(input.cq)[4:length(input.cq)] == type)){
      stop(paste("Cq Type:", type, "not found in input.cq"))
    }
  }

  # initialize Genotypes
  if(!any(unique(input.cq$type) == target)){
    stop("Target genotype not present!")
  }
  if(!length(unique(input.cq$type)) == 2){
    stop("More then two genotypes present. Check spelling?")
  }
  offtarget <- "Genotype B"   # not needed
  for(genotype in unique(input.cq$type)) {
    if (genotype != target) {
      offtarget <- genotype
    }
  }

  # get the wells for the sample:
  containing.A <- as.character(input.cq$well[input.cq$sample == sample & input.cq$type == target])
  containing.B <- as.character(input.cq$well[input.cq$sample == sample & input.cq$type == offtarget])


  ## get the maximal amount of samples.
  data.length <- max(length(containing.A), length(containing.B))


  # generate empty result table
  result.table <- data.frame(matrix(ncol=0, nrow=data.length))

  for (type.n in CqType) {
    containing.A.value <- eval(parse(text = paste0("input.cq$", type.n, "[input.cq$well %in% containing.A & input.cq$sample == '", sample,"']")))
    containing.B.value <- eval(parse(text = paste0("input.cq$", type.n, "[input.cq$well %in% containing.B & input.cq$sample == '", sample,"']")))

    # Outlier detection:
    if (outliers){
      if (startsWith(tolower(outliers.method), "d")){
      containing.A.value <- voges_dixon(data = containing.A.value, outlier.range = outlier.range, alpha = alpha, silent = silent)
      containing.B.value <- voges_dixon(data = containing.B.value, outlier.range = outlier.range, alpha = alpha, silent = silent)
      } else if (startsWith(tolower(outliers.method), "g")){
        containing.A.value <- voges_grubbs(data = containing.A.value, outlier.range = 6, alpha = alpha, silent = silent)
        containing.B.value <- voges_grubbs(data = containing.B.value, outlier.range = 6, alpha = alpha, silent = silent)
      } else {
        stop("No such outlier method... enter 'Grubbs' or 'Dixon'.")
      }
    }

    # make Sure they have the same length to paste them into the results!
    if (length(containing.A.value) < data.length) {
      miss <- data.length - length(containing.A.value)
      containing.A.value <- c(containing.A.value, c(rep(NA,miss)))
    }
    if (length(containing.B.value) < data.length) {
      miss <- data.length - length(containing.B.value)
      containing.B.value <- c(containing.B.value, c(rep(NA,miss)))
    }

    result.table[,paste0(type.n, ".", target)] <- containing.A.value
    result.table[,paste0(type.n, ".", offtarget)] <- containing.B.value
  }

  # leave function if only dataframe is wanted!
  if (tolower(format) == "data"){
    return(result.table)
  }

  # Calculate mean and sd:
  rows <- nrow(result.table)
  result.table <- rbind(result.table, mean = lapply(result.table[1:rows,], mean, na.rm=TRUE))
  result.table <- rbind(result.table, sd = lapply(result.table[1:rows,], sd, na.rm=TRUE))

  # Calculate Differences:
  for (type.n in CqType) {
    result.table[,paste0("diff.", type.n)] <- as.numeric(c(rep(NA,data.length),result.table["mean",paste0(type.n,".",target)]-result.table["mean",paste0(type.n,".",offtarget)], result.table["sd",paste0(type.n,".",target)]+result.table["sd",paste0(type.n,".",offtarget)]))
  }

  # leave function if only dataframe is wanted!
  if (tolower(format) == "fulldata"){
    return(result.table)
  }

  #Format the Diffs in a copied table
  result.table.format <- round(result.table, decimals)

  coln <- ncol(result.table.format)
  rown <- nrow(result.table.format)
  for (type.n in 1:length(CqType)) {
    result.table.format[(rown-1):rown, (coln-type.n+1)] <- cell_spec(result.table.format[(rown-1):rown, (coln-type.n+1)], color="white", background = "green")
  }

  # reorder:  (This could be better designed!)
  order <- length(CqType)
  if (order == 1) {
    sort <- c(1,2,3)
    border <- c(1)
  } else if (order == 2) {
    sort <- c(1,2,5,3,4,6)
    border <- c(1,4)
  } else if (order == 3) {
    sort <- c(1,2,7,3,4,8,5,6,9)
    border <- c(1,4,7)
  } else if (order == 4) {
    sort <- c(1,2,9,3,4,10,5,6,11,7,8,12)
    border <- c(1,4,7,10)
  } else if (order == 5) {
    sort <- c(1,2,11,3,4,12,5,6,13,7,8,14,9,10,15)
    border <- c(1,4,7,10,13)
  } else if (order == 6) {
    sort <- c(1,2,13,3,4,14,5,6,15,7,8,16,9,10,17,11,12,18)
    border <- c(1,4,7,10,13,16)
  } else {
    warning("sorry does only work with 6 or less types!")
    return()
  }
  result.table.format <- result.table.format[colnames(result.table)[sort]]

  # return the table
  if (tolower(format) == "kable") {
    options(knitr.kable.NA = '')
    kable(result.table.format, caption = paste0("Cq-values: ", sample, " ", target), escape = FALSE) %>%
      # kable_styling(bootstrap_options = c("striped", "hover", "condensed"))%>%
      kable_paper("hover")%>%
      row_spec(nrow(result.table.format)-1,bold=T, hline_after = FALSE)%>%
      row_spec(nrow(result.table.format),bold=T, hline_after = TRUE)%>%
      row_spec(0, font_size = 10)%>%
      column_spec(border, border_right = T)
  } else if (tolower(format) == "dt") {
    datatable(result.table,
              caption = paste0("Cq-values: ", sample, " ", target),
              rownames = TRUE,
              extensions = 'Buttons',
              class = 'cell-border stripe',
              options = list(
                searching = FALSE,
                buttons = c('copy', 'csv', 'excel'),
                dom = 'rtB',
                ordering = FALSE
              ))%>%
      formatRound(columns=c(1:ncol(result.table)), digits=decimals) %>%
      formatStyle(
        0,
        target = "row",
        fontWeight = styleEqual(c("mean","sd"), c("bold","bold"))
      )
  } else {
    result.table <- result.table[colnames(result.table)[sort]]
    return(result.table)
  }
}

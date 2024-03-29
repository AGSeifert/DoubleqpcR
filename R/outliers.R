#' Helper function for outlier detection.
#'
#' Wrapper for the Dixon test from outliers package.
#'
#' @import outliers
#' @param data input data (List, dataframe or vector?)
#' @param alpha alpha for outlier testing (0.05 = 95% significance) For small samples, this can good to be 90%!
#' @param outlier.range This is only important for samples with 3 or less values. In this case the range of data (e.g. Range c(1,1.4,1.3) = 0.4) need to be at least outlier.range if an outlier test should be done. Normally outlier test for 3 or less values is not recommended. But this helps to get rid of clear outliers e.g. (2,2,30). My advice is to check the data also manually.
#' @param silent should outliers be printed to output?
#' @return returns a dataframe or list depending on the input.
#' @export

voges_dixon <- function(data, outlier.range = 3, alpha = 0.05, silent = FALSE) {

  if (length(na.omit(data)) >= 3){  # if there are less datapoints than 3.
    if (length(unique(data)) > 1) { # if there is only one unique value. The test can't be done.
      if (length(unique(data)) >= 3 || max(data)-min(data) >= outlier.range) { # if more than 2 unique values, or: 2 unique!, but the range is higer outlier range.
        dixon.result <- dixon.test(data, type = 0, opposite = FALSE)
        if (as.numeric(dixon.result$p.value) < alpha) {
          if(!silent){
            print(paste(outlier(data), "in", paste(data, collapse = " "), "is an outlier."))
          }
          data[data == outlier(data)] <- NA
        }
    }}}

  return(data)
}

#' Helper function for outlier detection.
#'
#' Wrapper for the Grubbs test from outliers package. This test will scan for multiple outliers! removing one after another.
#'
#' @import outliers
#' @param data input data (List, dataframe or vector?)
#' @param alpha p-value for outlier removal (0.05 = 95% significance)
#' @param outlier.range How many values have to be present for doing grubbs test (std 6)
#' @param silent should outliers be printed to output?
#' @return returns a dataframe or list depending on the input.
#' @export
voges_grubbs <- function(data, outlier.range = 6, alpha = 0.05, silent = FALSE) {

  out.test <- TRUE

  while (out.test == TRUE) {
    if (length(na.omit(data)) < outlier.range){
      out.test <- FALSE
    } else {
      test <- grubbs.test(data)
      if (test$p.value < alpha) {
        if(!silent){
          print(paste(outlier(data), "in", paste(data, collapse = " "), "is an outlier."))
        }
        data[data == outlier(data)] <- NA
      } else {
        out.test <- FALSE
      }
    }
  }
  return(data)
}

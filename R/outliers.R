#' Helper function for outlier detection.
#'
#' Wrapper for the Dixon test from outliers package.
#'
#' @import outliers
#' @param data input data (List, dataframe or vector?)
#' @param alpha alpha for outlier testing (0.05 = 95% significance)
#' @param outlier.range This is only important for samples with 3 or less values. In this case the range of data (e.g. Range c(1,1.4,1.3) = 0.4) need to be at least outlier.range if an outlier test shoud happen. Normally outlier test for 3 or less values is not recommended. But this helps to get rid of clear outliers e.g. (2,2,30). My advice is to check the data also manually.
#' @param silent should outliers be printed to output?
#' @return returns a dataframe or list depending on the input.
#' @export

voges_dixon <- function(data, outlier.range = 3, alpha = 0.05, silent = FALSE) {

  if (length(na.omit(data)) >= 3){  # if there are less datapoints than 3, just go on!
    if (length(unique(data)) > 1) { # if there is only one uniqe value. The test can't be done.
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

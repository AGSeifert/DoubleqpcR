#' Load fluorescence table
#'
#' This function is a wrapper for read.csv
#' #'
#' @param csv Path to the input csv table
#' @param sep cell separator "," "." or ...
#' @param dec decimals as "." or "," or ...
#' @return returns a data frame of fluorescence values.
#' @export
read.fluorescenceTable <- function(csv, sep = ",", dec=","){
  input <- read.csv(csv, sep=sep, dec=dec, numerals = "warn.loss")
  return(input)
}

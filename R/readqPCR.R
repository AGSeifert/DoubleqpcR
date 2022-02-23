#' Load fluorescence table
#'
#' This function is a wrapper for read.csv
#' The input.raw and input.raw.melt data frames will be saved to global scope.
#' Rigth now you only can have 1 table for 1 analyis. You can consider to add more values manually or combine the csv files beforehand. Make sure the wells are named individually.
#' The first column need to be called "Cycle"!
#'
#' @import reshape2
#' @param csv Path to the input csv table
#' @param sep cell separator "," "." or ...
#' @param dec decimals as "," or "." or ...
#' @return does not return! input.raw and input.raw.melt data frames will be saved to global.
#' @export
read.fluorescenceTable <- function(csv, sep = ",", dec=","){
  input.raw <<- read.csv(csv, sep=sep, dec=dec, numerals = "warn.loss", na.strings=c("NA","N/A", " "))

  if (colnames(input.raw[1]) != "Cycle"){
    warning("Table does not have a 'Cycle' column in 1. pos. Renaming first col 'Cycle'")
    colnames(input.raw)[1] <<- "Cycle"
  }

  input.raw.melt <<- melt(input.raw, id.vars = "Cycle")
  if(exists("input.cq")){
    input.raw.melt.usedOnly <<- input.raw.melt[input.raw.melt$variable %in% input.cq$well,]
  } else {
    warning("No input.cq found. Therfore .usedOnly Table is only a copy from input.raw.melt! Rerun after read.cqTable()!")
    input.raw.melt.usedOnly <<- input.raw.melt
  }
}


#' Load cq table
#'
#' This function is a wrapper for read.csv
#'
#' First three needs to be 'type','sample','well'
#' Type is the genotype or refers to the 2 primers. There have to be only 2!
#' Sample is the analysed concentration, or experiment. If you want to use the regression fct. these need to be numbers (eg. 0%-100%)
#' Well is the well, and it need to be the same as in the fluorescence data, if used.
#'
#' The following columns are the Cq vales. The column name will be used as Cq name reference.
#'
#' @param csv Path to the input csv table
#' @param sep cell separator "," "." or ...
#' @param dec decimals as "," or "." or ...
#' @return does not return! input.cq data frame will be saved to global.
#' @export
read.cqTable <- function(csv, sep = ",", dec=","){

  input.cq <<- read.csv(csv, sep=sep, dec=dec, numerals = "warn.loss", na.strings=c("NA","N/A", " "))

  if (colnames(input.cq[1]) != "type" | colnames(input.cq[2]) != "sample" | colnames(input.cq[3]) != "well" ){
    warning("Table does not colnames correct! First three needs to be 'type','sample','well'. Renaming, but not checking anything! Please manually check.")
    colnames(input.cq)[1:3] <<- c("type", "sample", "well")
  }

  if (length(unique(input.cq$type)) != 2) {
    warning("There are not two genotypes present in the data. This may cause massive trouble! Maybe misspelled?")
  }

}

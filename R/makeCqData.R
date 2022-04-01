#' This Function will create the main Data object for all analysis.
#' Used after an input is created with read.CqTable and all optional calculation data validation is complete.
#'
#' Outliers can be removed. To check the results one can use the table.Cq() function before.
#'
#' If an data.Cq object already exists, it will be overwritten when add = FALSE.
#' Otherwise samples will be added. Or overwritten! It is not jet possible to add more values in a sample...
#'
#' @param add This toggle wil add the samples, if an data.cq is already existing in global scope.
#' @param target the target genotype "genotype A".
#' @param CqType this is the Cq value columns from the input.cq that should be used.
#' @param outliers logical if outliers are to be deleted from the output
#' @param outliers.method If a "Dixon" or "Grubbs" test should be used.
#' @param alpha alpha for outlier testing (0.05 = 95% significance)
#' @param outlier.range For Grubbs: input ignored, set to 6. For Dixon: This is only important for samples with 3 or less values. In this case the range of data (e.g. Range c(1,1.4,1.3) = 0.4) need to be at least outlier.range if an outlier test should happen. Normally outlier test for 3 or less values is not recommended. But this helps to get rid of clear outliers e.g. (2,2,30). My advice is to check the data also manually.
#' @param silent If status of outlier detection and processing is printed.
#' @return returns a list of samples with cq values (data.cq)
#' @export
make.Cq.data <- function(add = FALSE, target = "Genotype A", CqType = c("TP","SD"), outliers = TRUE, outliers.method = "Grubbs", alpha = 0.05, outlier.range = 3, silent = FALSE){

  # Checks if data can be added!
  if(add){
    if(!exists("data.Cq")){
      warning("The data.Cq object is not found. Create new data.Cq!")
      add <- FALSE
    }
  }
  #create new data.cq
  if(!add){
    data.Cq <<- list()
  }

  # Go over all samples and Cq types in input.cq:
  for (sample in unique(input.cq$sample)) {
    sample.list <- list()
    if(!silent){
      print(paste0("Sample ", sample, " :"))
    }
    for (type in CqType) {
      if(!silent){
        print(paste0("Cq type ", type, " processing:"))
      }
      cqVals <- table.Cq(sample = sample, target = target, CqType = type, outliers = outliers, outliers.method = outliers.method, alpha = alpha, format = "data", outlier.range = outlier.range, silent = silent)
      eval(parse(text = paste0("sample.list$`", type,"` <- cqVals")))
    }

    eval(parse(text = paste0("data.Cq$`", sample,"` <<- sample.list")))
  }

}

#' This Function will combine subsamples, so that they can be used as one sample.
#'
#' Normaly make.Cq.data() was called with outlier removal for a subsample. Or just to combine different input tables.
#' In order to do this different sample names were needed and now it is time to combine these individual samples back together.
#'
#' This function will combine subsample with the same base name before a delimiter.
#' e.g. "100.1, 100.2, 100.a, 100.something" would be in the same sample "100" after.
#'
#' @param delimiter standard: "." wich seperates the base name from the subsample counter or name. "." can be problematic for decimal values. The FIRST Occourance of this delimiter will be used only. Everything after is trimmed.
#' @param outliers logical if outliers are to be deleted from the output
#' @param outliers.method If a "Dixon" or "Grubbs" test should be used.
#' @param alpha alpha for outlier testing (0.05 = 95% significance)
#' @param outlier.range For Grubbs: input ignored, set to 6. For Dixon: This is only important for samples with 3 or less values. In this case the range of data (e.g. Range c(1,1.4,1.3) = 0.4) need to be at least outlier.range if an outlier test should happen. Normally outlier test for 3 or less values is not recommended. But this helps to get rid of clear outliers e.g. (2,2,30). My advice is to check the data also manually.
#' @return nothing. changes data.Cq in global scope
#' @export
combineSubsamples <- function(delimiter = ".", outliers = TRUE, outliers.method = "Grubbs", alpha = 0.05, outlier.range = 3, silent = FALSE){
  data <- list()
  for (sample in names(data.Cq)) {
    newsample <- trimws(sample, which = "r", whitespace = paste0("[", delimiter, "].*"))
    list <- list()
    list[[1]] <- eval(parse(text=paste0("data.Cq$'",sample,"'"))) # list of dataframes for all Cq Types.
    names(list) <- newsample
    if(newsample %in% names(data)){
      for (df in names(eval(parse(text=paste0("data$'",newsample,"'"))))) {
        # This would be for lists:
        # eval(parse(text=paste0("data$'",newsample,"'$",df," <- Map(c, data$'",newsample,"'$",df, ", list[[1]]$",df,")")))
        eval(parse(text = paste0("data$'",newsample,"'$",df," <- rbind(data$'",newsample,"'$",df,", list[[1]]$",df,")")))
      }
    } else {
      data <- append(data, list)
    }
  }
  if(outliers){
    for (sample in names(data)) {
      for(list in names(eval(parse(text=paste0("data$'",sample,"'"))))){
        if (startsWith(tolower(outliers.method), "d")){
          eval(parse(text=paste0("data$'",sample,"'$",list,"[[1]] <- voges_dixon(data = data$'",sample,"'$",list,"[[1]], outlier.range = outlier.range, alpha = alpha, silent = silent)")))
          eval(parse(text=paste0("data$'",sample,"'$",list,"[[2]] <- voges_dixon(data = data$'",sample,"'$",list,"[[2]], outlier.range = outlier.range, alpha = alpha, silent = silent)")))
        } else if (startsWith(tolower(outliers.method), "g")){
          eval(parse(text=paste0("data$'",sample,"'$",list,"[[1]] <- voges_grubbs(data = data$'",sample,"'$",list,"[[1]], outlier.range = 6, alpha = alpha, silent = silent)")))
          eval(parse(text=paste0("data$'",sample,"'$",list,"[[2]] <- voges_grubbs(data = data$'",sample,"'$",list,"[[2]], outlier.range = 6, alpha = alpha, silent = silent)")))
        } else {
          stop("No such outlier method... enter 'Grubbs' or 'Dixon'.")
        }
      }
    }
  }
  data.Cq <<- data
}

#' This Function will summarize the data.cq samples for one Cq type!
#' - helper function.
#'
#' @param CqType this is the Cq value columns from the input.cq that should be used.
#' @param onlyNumeric Will only use samples that are a numerical (for percentages the "%" will be stripped)
#' @param return standard=FALSE will write in global scope! Otherwise will return the dataframe.
#' @return standard: returns nothing. Creates a dataframe or creates a a dataframe data.cq.sum in global scope
#' @export
Cq.data.mean <- function(CqType = "SD", onlyNumeric = FALSE, return = FALSE){

  if(!exists("data.Cq")){ # Check if data.cq is available
    stop("No data.cq list available - please run make.cq.data()")
  }

  if(length(CqType) != 1){ # Check if CqType is only one type!
    stop("Cq Type needs to be only one!")
  }

  # new data frame for return:
  df <- data.frame(sample = character(), mean.Cq.target = numeric(), sd.Cq.target = numeric(), mean.Cq.offtarget = numeric(), sd.Cq.offtarget = numeric())

  # Go over all samples (maybe in apply function better?)
  for (sample in names(data.Cq)) {
    if(onlyNumeric){
      sample <- trimws(gsub("%","",sample))
      thisSample <- try(as.numeric(sample), silent = TRUE)  # will be NA if not numerical conversion possible.
      if(is.na(thisSample)){  # leave sample out, if it is not numeric!
        next
      } else {
        sample <- thisSample
      }
    }
    df <- rbind(df, data.frame(sample = sample,
                 mean.Cq.target = mean(eval(parse(text = paste0("data.Cq$'", sample, "'$", CqType, "[,1]"))), na.rm = TRUE),
                 sd.Cq.target = sd(eval(parse(text = paste0("data.Cq$'", sample, "'$", CqType, "[,1]"))), na.rm = TRUE),
                 mean.Cq.offtarget = mean(eval(parse(text = paste0("data.Cq$'", sample, "'$", CqType, "[,2]"))), na.rm = TRUE),
                 sd.Cq.offtarget = sd(eval(parse(text = paste0("data.Cq$'", sample, "'$", CqType, "[,2]"))), na.rm = TRUE)
                 ))

  }
  if(return){
    return(df)
  } else{
    data.Cq.sum <<- df
  }
}

#' This Function will give the data.cq samples for one Cq type in a dataframe
#' - helper function.
#'
#' @param CqType this is the Cq value columns from the input.cq that should be used.
#' @param onlyNumeric Will only use samples that are a numerical (for percentages the "%" will be stripped)
#' @param return standard=FALSE will write in global scope! Otherwise will return the dataframe.
#' @return standard: returns nothing. Creates a dataframe or creates a a dataframe data.cq.sum in global scope
#' @export
Cq.data.df <- function(CqType = "SD", onlyNumeric = FALSE, return = FALSE){

  if(!exists("data.Cq")){ # Check if data.cq is available
    stop("No data.cq list available - please run make.cq.data()")
  }

  if(length(CqType) != 1){ # Check if CqType is only one type!
    stop("Cq Type needs to be only one!")
  }

  # new data frame for return:
  df <- data.frame(sample = character(), Cq.target = numeric(), Cq.offtarget = numeric())

  # Go over all samples (maybe in apply function better?)
  for (sample in names(data.Cq)) {
    if(onlyNumeric){
      sample <- trimws(gsub("%","",sample))
      thisSample <- try(as.numeric(sample), silent = TRUE)  # will be NA if not numerical conversion possible.
      if(is.na(thisSample)){  # leave sample out, if it is not numeric!
        next
      } else {
        sample <- thisSample
      }
    }
    df <- rbind(df, data.frame(sample = sample,
                               Cq.target = eval(parse(text = paste0("data.Cq$'", sample, "'$", CqType, "[,1]"))),
                               Cq.offtarget = eval(parse(text = paste0("data.Cq$'", sample, "'$", CqType, "[,2]")))
    ))

  }
  if(return){
    return(df)
  } else{
    data.Cq.df <<- df
  }
}


#' This Function will give the delta Cq values. Always: offtarget Cq - target Cq
#' - helper function.
#'
#' The delta Cq values can be calculated by randomly assign two values, build all combinations of deta Cq. Directly use the values from the same row or just use the mean values.
#'
#' @param CqType this is the Cq value columns from the input.cq that should be used.
#' @param method can be either "combinatorial", "random", "direct" or "mean" (can be abbriviated). This will impact how the differences are calculated.
#' @param onlyNumeric Will only use samples that are a numerical (for percentages the "%" will be stripped)
#' @param return standard=FALSE will write in global scope! Otherwise will return the dataframe.
#' @return standard: returns nothing. Creates a dataframe or creates a a dataframe data.cq.sum in global scope
#' @export
delta.Cq.data <- function(CqType = "SD", method = "combinatorial", onlyNumeric = FALSE, return = FALSE){

  data <- Cq.data.df(CqType = CqType, onlyNumeric = onlyNumeric, return = TRUE)

  # make all combinations: (neglect NA values)
  if (startsWith(tolower(method),"c")){
    data.list <- split(data, data$sample)
    data <- lapply(data.list, function(x){
      combinations <- expand.grid(x[,3], x[,2])
      combinations$Var1 - combinations$Var2
    })
    df <- data.frame(sample = character(), delta.Cq = numeric())
    for (i in 1:length(data)) {
      d <- data.frame(sample = rep(names(data[i]), length(data[[i]])), delta.Cq = data[[i]])
      df <- rbind(df, d)
    }
    df <- df[complete.cases(df$delta.Cq),]
  # only mean values
  } else if(startsWith(tolower(method),"m")){
    data.list <- split(data, data$sample)
    data <- sapply(data.list, function(x){
      mean(x[,3], na.rm = TRUE) - mean(x[,2], na.rm = TRUE)
    })
    df <- data.frame(sample = names(data), delta.Cq = data)
  # randomly assigned
  } else if(startsWith(tolower(method),"r")){
    data.list <- split(data, data$sample)
    data <- lapply(data.list, function(x){
      sample(x[,3]) - sample(x[,2])
    })
    df <- data.frame(sample = character(), delta.Cq = numeric())
    for (i in 1:length(data)) {
      d <- data.frame(sample = rep(names(data[i]), length(data[[i]])), delta.Cq = data[[i]])
      df <- rbind(df, d)
    }
    df <- df[complete.cases(df$delta.Cq),]
  # directly assigned
  } else if(startsWith(tolower(method),"d")){
    #df <- data.frame(sample = data$sample, delta.Cq = data[,3] - data[,2])
    #df <- df[complete.cases(df$delta.Cq),]
    data.list <- split(data, data$sample)
    data <- lapply(data.list, function(x){
      x[,3] - x[,2]
    })
    df <- data.frame(sample = character(), delta.Cq = numeric())
    for (i in 1:length(data)) {
      d <- data.frame(sample = rep(names(data[i]), length(data[[i]])), delta.Cq = data[[i]])
      df <- rbind(df, d)
    }
    df <- df[complete.cases(df$delta.Cq),]
  } else {
      stop("Sorry, no valid method for calculating delta Cq values.")
  }

  if(onlyNumeric){
    # This was a test for remove the sample factors?
    #delta.Cq <- delta.Cq[order(delta.Cq$sample),]
    #levels(df$sample) <- sort(as.numeric(levels(df$sample)))
    df$sample <- as.numeric(as.character(df$sample))
  }


  if(return){
    return(df)
  } else{
    delta.Cq <<- df
  }
}

#' This function wil perform a regression analysis on the delta Cq values.
#'
#' The model and prediction with investR package are reported. A CrossValidation for the model is possible with caret.
#' The cross validation here is a basic wrapper for caret package. If more detailed CV is wanted, the analysis should be made separately with caret.
#'
#' @importFrom caret trainControl train
#' @import investr
#' @param CqType this is the Cq value type that should be used.
#' @param fit model for lm() function. "linear", "poly3", "poly4".
#' @param method method for generating the delta Cq values (see delta.Cq.data())
#' @param cv should a cross validation be made? With caret!
#' @param cv.seed seed for cv
#' @param cv.method The method for cv from caret package
#' @param cv.p percentage of training data
#' @param plot plot the data with plotfit() method from investR - std setting (for more optiones use plotfit() seperatly)
#' @return returns a model: model.delta.Cq object in global space.
#' @export
regression.delta.Cq <- function(CqType = "SD", method = "c", fit = "linear", rawPolynomials = FALSE, cv = FALSE, cv.seed = sample(1:100, 1), cv.method = "LGOCV", cv.p = 50, plot = TRUE){

  if(!exists("data.Cq")){ # Check if delta.Cq is available
    stop("No data.cq list available - please run make.Cq.data()")
  }

  this.delta.Cq <- delta.Cq.data(CqType = CqType, method = method, onlyNumeric = TRUE, return = TRUE)

  method <- tolower(gsub(" ","", fit)) #just remove white spaces and lower case
  if (startsWith(method, "l")) {
    # Linear model
    func <- formula(delta.Cq ~ sample)
  } else if(method == "poly3"){
    # polinomial model 3
    func <- as.formula(paste0("delta.Cq ~ poly(sample,3, raw = ",rawPolynomials,")"))
  } else if(method == "poly4"){
    # polinomial model 4
   func <- as.formula(paste0("delta.Cq ~ poly(sample,4, raw = ",rawPolynomials,")"))
  } else {
    stop(paste("No method found for", fit))
  }

  model.delta.Cq <<- lm(data = this.delta.Cq, func)

  if (cv) {
    # for CV
    set.seed(cv.seed)
    # p should not be set if not group cv?
    ctrl <- caret::trainControl(method = cv.method, p = cv.p)
    train.model <- caret::train(func, data = this.delta.Cq, method = "lm", trControl = ctrl)
  }

  if (plot) {
  plotFit(model.delta.Cq, interval = "both",
          level = 0.95, shade = TRUE,
          col.pred = "whitesmoke",
          col.conf = "skyblue",
          extend.range = TRUE,
          xlab = "Proportion of target genotype [%]",
          ylab = bquote(Delta*Cq))
  }

}

#' This function will build a regression model on the delta Cq values. Plot and cross validation is included.
#'
#' The model and prediction with investR package are reported. A CrossValidation for the model is possible with caret.
#' The cross validation here is a basic wrapper for caret package. If more detailed CV is wanted, the analysis should be made separately with caret.
#'
#' @importFrom caret trainControl train
#' @import investr
#' @param CqType this is the Cq value type that should be used.
#' @param linSqrtTrans Will transform the values to linearise the values! this is basically a shifted square root representation. The parameter fit should be linar as well.
#' @param fit model for lm() function. "linear", "poly3", "poly4".
#' @param method method for generating the delta Cq values (see delta.Cq.data())
#' @param cv should a cross validation be made? With caret!
#' @param cv.seed seed for cv
#' @param cv.method The method for cv from caret package
#' @param cv.p percentage of training data (0 to 1)
#' @param cvComplete should a cross validation be made with omitting one concentration completely? This will run separately from caret cv.
#' @param plot plot the data with plotfit() method from investR with std. settings (for more options use plotfit() separately)
#' @param cv.plot plot the data with plotfit() for the cvComplete cross validation.
#' @return returns a model: model.delta.Cq object in global space.
#' @export
regression.delta.Cq <- function(CqType = "SD", linSqrtTrans = FALSE, method = "c", fit = "linear", rawPolynomials = FALSE, cv = FALSE, cv.seed = sample(1:100, 1), cv.method = "LGOCV", cv.p = 0.5, cvComplete = FALSE, plot = TRUE, cv.plot = FALSE){

  if(!exists("data.Cq")){ # Check if delta.Cq is available
    stop("No data.cq list available - please run make.Cq.data()")
  }

  modelData.delta.Cq <<- delta.Cq.data(CqType = CqType, method = method, onlyNumeric = TRUE, return = TRUE)

  # SHIFTED SQUARE ROOT TRANSFORMATION
  if (linSqrtTrans) {
    modelData.delta.Cq <<- linSqrtTransform(modelData.delta.Cq)
  }

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

  model.delta.Cq <<- lm(data = modelData.delta.Cq, func)

  if (cv) {
    # for CV
    set.seed(cv.seed)
    # p should not be set if not group cv?
    ctrl <- caret::trainControl(method = cv.method, p = cv.p)
    train.model <- caret::train(func, data = modelData.delta.Cq, method = "lm", trControl = ctrl)
    print("CROSS VALIDATION:")
    print(train.model)
  }

  if (plot) {
    if (linSqrtTrans) {xl <- "Shifted square root of target proportion"} else {xl <- "Proportion of target genotype [%]"}
    plotFit(model.delta.Cq, interval = "both",
          level = 0.95, shade = TRUE,
          col.pred = "whitesmoke",
          col.conf = "skyblue",
          extend.range = TRUE,
          xlab = xl,
          ylab = bquote(Delta*Cq))
  }

  if (cvComplete){

    # Template for the resulting fataframe
    test.df <- data.frame(conc=numeric(), estimate.percentage=numeric(), low.dev=numeric(), up.dev=numeric(), error = numeric())

    for (cvc in unique(modelData.delta.Cq$sample)) {
        # modify data (leave one group out and sets its mean as test data.)
        data.cvc <- modelData.delta.Cq[modelData.delta.Cq$sample != cvc,]
        data.cvc.testCq <- mean(modelData.delta.Cq$delta.Cq[modelData.delta.Cq$sample == cvc])
        # Make a new model for this set
        model.delta.Cq.cvc <- lm(data = data.cvc, func)

        res.cvc <- invest(model.delta.Cq.cvc, y0 = data.cvc.testCq, interval = "inversion", level = 0.95, mean.response = TRUE, upper = 200, lower = -200)

        if (cv.plot) {
        plotFit(model.delta.Cq.cvc, interval = "both",
                level = 0.95, shade = TRUE,
                col.pred = "whitesmoke",
                col.conf = "skyblue",
                extend.range = TRUE,
                xlab = "Percentage of target genotype [%]",
                ylab = bquote(Delta~Cq))
        abline(h = data.cvc.testCq, v = c(res.cvc$lower, res.cvc$estimate, res.cvc$upper), lty = 2, lwd = 0.5, col = "salmon")
        }

        test.df <- rbind(test.df, data.frame(conc=cvc, estimate.percentage=res.cvc$estimate, low.dev=res.cvc$estimate-res.cvc$lower, up.dev=res.cvc$upper-res.cvc$estimate))
    }
    # if squarte root trans: make the sqrt to normal percentage:
    if (linSqrtTrans){
      test.df$conc <- reTransform(test.df$conc)
      test.df$estimate.percentage <- reTransform(test.df$estimate.percentage)
   }

    # report the results
    test.df$error <- abs(test.df$estimate.percentage - test.df$conc)
    writeLines("\n=====\nLeave one proportion completely out - mean cross validation error [percentage points]:")
    print(mean(test.df$error))
    model.complete.cv <<-test.df
  }

}

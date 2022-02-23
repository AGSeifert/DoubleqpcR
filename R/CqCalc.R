#' calculates Cq values
#'
#' This function will calculate Cq values for the data frame input.cq
#' You need to have input.cq and input.raw from "read.fluorescenceTable" / "read.cqTable" or build them manually.
#'
#' !This will fail sometimes, do to uniroot() function returning 0!
#'
#'
#' @importFrom drc drm
#' @importFrom drc LL.5
#' @importFrom drc LL.4
#' @param cq.new The Column name of the new Cq values.
#' @param method The method used to calculate the Cq value. "TP" for Turning Point (first derivative) . "SD" for first exponential incline (second derivative).
#' @param fct will be used by {drc} as fitting function. "ll4" or "ll5" is implemented.
#' @return does not return! adds Cq value to input.cq data frame
#' @export
calc.Cq <- function(method = "TP", fct = "ll5", cq.new = "calc.Cq"){
  # This will iterate over all wells that are in the Cq table:
  result <- c()
  # make APPLY FUNCTION?
  for (well in input.cq$well) {
    form <- as.formula(paste(well, "~ Cycle"))
    # LL.5 Modell will be used. The Formula for LL.5 is:  $f(x) = c + \frac{d-c}{(1+\exp(b(\log(x)-\log(e))))^f}$
    if (fct == "ll4") {
      func <- LL.4()
    } else {
      func <- LL.5()
    }
    lm_cq <- drm(form, fct = func, data = input.raw)

    b <- as.numeric(lm_cq$coefficients["b:(Intercept)"])
    c <- as.numeric(lm_cq$coefficients["c:(Intercept)"])
    d <- as.numeric(lm_cq$coefficients["d:(Intercept)"])
    e <- as.numeric(lm_cq$coefficients["e:(Intercept)"])
    if (fct == "ll4") {
      fu <- paste0(c,"+((",d,"-",c,")/((1+exp(",b,"*(log(x)-log(",e,"))))))")
    } else {
      f <- as.numeric(lm_cq$coefficients["f:(Intercept)"])
      fu <- paste0(c,"+((",d,"-",c,")/((1+exp(",b,"*(log(x)-log(",e,"))))^",f,"))")
    }

    fu <- parse(text = fu)
    if (method == "TP"){
      fu.d.d <- D(D(fu, "x"), "x")
    } else if (method == "SD") {
      fu.d.d <- D(D(D(fu, "x"), "x"), "x")
    } else {
      stop(paste("no method ", method ,"found!"))
    }

    func <- function(x){}
    body(func) <- fu.d.d

    if (method == "TP"){
      cq <- uniroot(func, c(1,50), extendInt="yes")$root
    } else if (method == "SD") {
      cq <- uniroot(func, c(1,10), extendInt="yes")$root
    }

    if (length(cq) > 1) {
      cq <- min(cq)
      warning("oh no, there was more than one zero point. I took the lowest")
    }
    result <- c(result, cq)
  }

  input.cq[ncol(input.cq)+1] <<- round(result,2)
  colnames(input.cq)[ncol(input.cq)] <<- cq.new
}


#' calculates Cq values
#'
#' WRAPPER for qpcR Package
#' 'qpcR: Modelling and Analysis of Real-Time PCR Data, Andrej-Nikolai Spiess, 2018'
#'
#' This function will calculate Cq values for the data frame input.cq
#' You need to have input.cq and input.raw from "read.fluorescenceTable" / "read.cqTable" or build them manually.
#'
#' Please note, that it is faster to use qpcR directly!
#'
#' @import qpcR
#' @param cq.new The Column name of the new Cq values.
#' @param method The method used to calculate the Cq value. "TP / cpD1" for Turning Point (first derivative) . "SD / cpD2" for first exponential incline (second derivative). "cpE", "cpR", "cpT", "Cy0", "cpCQ", "cpMR" (see qpcR package)
#' @param fct will be used by qpcR as fitting function. l4, l5, b4
#' @return does not return! adds Cq value to input.cq data frame
#' @export
calc.Cq.qpcR <- function(method = "TP", fct = l4, cq.new = "calc.qpcR.Cq"){

  # This will iterate over all wells that are in the Cq table:
  result <- c()
  # make APPLY FUNCTION?
  for (well in input.cq$well) {
    cq <- efficiency(pcrfit(data = input.raw, cyc = 1, fluo = well, model = fct), plot = FALSE)
    if (method == "TP" | method == "cpD1") {
      result <- c(result, cq$cpD1)
    } else if (method == "SD" | method == "cpD2") {
      result <- c(result, cq$cpD2)
    } else if (method == "cpE") {
      result <- c(result, cq$cpE)
    } else if (method == "cpR") {
      result <- c(result, cq$cpR)
    } else if (method == "cpT") {
      result <- c(result, cq$cpT)
    } else if (method == "cy0" | method == "cyO") {
      result <- c(result, cq$cy0)
    } else if (method == "cpMR") {
      result <- c(result, cq$cpMR)
    } else {
      stop("method unknown!")
    }
  }

  input.cq[ncol(input.cq)+1] <<- round(result,2)
  colnames(input.cq)[ncol(input.cq)] <<- cq.new
}

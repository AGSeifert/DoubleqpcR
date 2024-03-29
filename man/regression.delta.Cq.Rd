% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Regression.R
\name{regression.delta.Cq}
\alias{regression.delta.Cq}
\title{This function will build a regression model on the delta Cq values. Plot and cross validation is included.}
\usage{
regression.delta.Cq(
  CqType = "SD",
  linSqrtTrans = FALSE,
  method = "c",
  fit = "linear",
  rawPolynomials = FALSE,
  cv = FALSE,
  cv.seed = sample(1:100, 1),
  cv.method = "LGOCV",
  cv.p = 0.5,
  cvComplete = FALSE,
  plot = TRUE,
  cv.plot = FALSE
)
}
\arguments{
\item{CqType}{this is the Cq value type that should be used.}

\item{linSqrtTrans}{Will transform the values to linearise the values! this is basically a shifted square root representation. The parameter fit should be linar as well.}

\item{method}{method for generating the delta Cq values (see delta.Cq.data())}

\item{fit}{model for lm() function. "linear", "poly3", "poly4".}

\item{cv}{should a cross validation be made? With caret!}

\item{cv.seed}{seed for cv}

\item{cv.method}{The method for cv from caret package}

\item{cv.p}{percentage of training data (0 to 1)}

\item{cvComplete}{should a cross validation be made with omitting one concentration completely? This will run separately from caret cv.}

\item{plot}{plot the data with plotfit() method from investR with std. settings (for more options use plotfit() separately)}

\item{cv.plot}{plot the data with plotfit() for the cvComplete cross validation.}
}
\value{
returns a model: model.delta.Cq object in global space.
}
\description{
The model and prediction with investR package are reported. A CrossValidation for the model is possible with caret.
The cross validation here is a basic wrapper for caret package. If more detailed CV is wanted, the analysis should be made separately with caret.
}

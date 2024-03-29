% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outliers.R
\name{voges_dixon}
\alias{voges_dixon}
\title{Helper function for outlier detection.}
\usage{
voges_dixon(data, outlier.range = 3, alpha = 0.05, silent = FALSE)
}
\arguments{
\item{data}{input data (List, dataframe or vector?)}

\item{outlier.range}{This is only important for samples with 3 or less values. In this case the range of data (e.g. Range c(1,1.4,1.3) = 0.4) need to be at least outlier.range if an outlier test shoud happen. Normally outlier test for 3 or less values is not recommended. But this helps to get rid of clear outliers e.g. (2,2,30). My advice is to check the data also manually.}

\item{alpha}{alpha for outlier testing (0.05 = 95\% significance)}

\item{silent}{should outliers be printed to output?}
}
\value{
returns a dataframe or list depending on the input.
}
\description{
Wrapper for the Dixon test from outliers package.
}

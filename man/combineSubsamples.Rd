% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeCqData.R
\name{combineSubsamples}
\alias{combineSubsamples}
\title{This Function will combine subsamples, so that they can be used as one sample.}
\usage{
combineSubsamples(
  delimiter = ".",
  useMeans = FALSE,
  outliers = TRUE,
  outliers.method = "Grubbs",
  alpha = 0.05,
  outlier.range = 3,
  silent = FALSE
)
}
\arguments{
\item{delimiter}{standard: "." wich seperates the base name from the subsample counter or name. "." can be problematic for decimal values. The FIRST Occourance of this delimiter will be used only. Everything after is trimmed.}

\item{useMeans}{instead of combining all values, only combine the mean values of each subsample.}

\item{outliers}{logical if outliers are to be deleted from the output}

\item{outliers.method}{If a "Dixon" or "Grubbs" test should be used.}

\item{alpha}{alpha for outlier testing (0.05 = 95\% significance)}

\item{outlier.range}{For Grubbs: input ignored, set to 6. For Dixon: This is only important for samples with 3 or less values. In this case the range of data (e.g. Range c(1,1.4,1.3) = 0.4) need to be at least outlier.range if an outlier test should happen. Normally outlier test for 3 or less values is not recommended. But this helps to get rid of clear outliers e.g. (2,2,30). My advice is to check the data also manually.}
}
\value{
nothing. changes data.Cq in global scope
}
\description{
Normaly make.Cq.data() was called with outlier removal for a subsample. Or just to combine different input tables.
In order to do this different sample names were needed and now it is time to combine these individual samples back together.
}
\details{
This function will combine subsample with the same base name before a delimiter.
e.g. "100.1, 100.2, 100.a, 100.something" would be in the same sample "100" after.
}

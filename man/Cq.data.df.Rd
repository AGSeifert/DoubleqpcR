% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeCqData.R
\name{Cq.data.df}
\alias{Cq.data.df}
\title{This Function will give the data.cq samples for one Cq type in a dataframe
\itemize{
\item helper function.
}}
\usage{
Cq.data.df(CqType = NA, onlyNumeric = FALSE, return = FALSE)
}
\arguments{
\item{CqType}{this is the Cq value columns from the input.cq that should be used.}

\item{onlyNumeric}{Will only use samples that are a numerical (for percentages the "\%" will be stripped)}

\item{return}{standard=FALSE will write in global scope! Otherwise will return the dataframe.}
}
\value{
standard: returns nothing. Creates a dataframe or creates a a dataframe data.cq.sum in global scope
}
\description{
This Function will give the data.cq samples for one Cq type in a dataframe
\itemize{
\item helper function.
}
}

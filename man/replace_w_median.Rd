% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprecessing.R
\name{replace_w_median}
\alias{replace_w_median}
\title{Fills NA's with the median of the column}
\usage{
replace_w_median(data, data_sub)
}
\arguments{
\item{data}{list of 2 lists; original train and test datasetes}

\item{data_sub}{list of 3 tibbles; all, save and rm}
}
\value{
list of 2 lists; attrs and data
\itemize{
  \item attrs - attributes to save and remove
  \item data - data for the model
}
}
\description{
Fills NA's with the median of the column
}

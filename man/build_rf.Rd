% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{build_rf}
\alias{build_rf}
\title{Breiman's random forest algorithm}
\usage{
build_rf(data, ntrees, mtry, importance)
}
\arguments{
\item{data}{list of data.frames; containing the training and testing sets for the model}

\item{ntrees}{int; number of trees to grow}

\item{mtry}{int; number of variables randomly sampled as candidates at each split}

\item{importance}{bool; should importance of predictors be assessed}
}
\value{
list of parameters created by the RandomForest model
}
\description{
Breiman's random forest algorithm
}

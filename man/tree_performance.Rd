% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_analyzer.R
\name{tree_performance}
\alias{tree_performance}
\title{tree_performance}
\usage{
tree_performance(tree, test_df, test_col)
}
\arguments{
\item{tree}{A data frame describing the decision tree.
This data frame should be in the same format as the trees used within the RF pacakge}

\item{test_df}{A data frame containing the test set.}

\item{test_col}{The name of the column in the test set to be used as the dependent variable.}
}
\value{
A list with three elements:
 \describe{
   \item{tree_info}{A data frame describing the decision tree, including information on how the test data is classified.
   In the format of trees used in teh random forest}
   \item{conf_mat}{A confusion matrix for the test data.}
   \item{accuracy}{The accuracy of the decision tree for the test data.}
 }
}
\description{
This function takes a decision tree which is described in a dataframe as
randomForest package outputs, takes a dataframe of a new test set and
returns predictions for each of those test sets and what node they land at
}

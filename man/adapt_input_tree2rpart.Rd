% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_analyzer.R
\name{adapt_input_tree2rpart}
\alias{adapt_input_tree2rpart}
\title{adapt_input_tree2rpart}
\usage{
adapt_input_tree2rpart(input_tree_df, add_data, X, y, model_type)
}
\arguments{
\item{input_tree_df}{the random forest tree object}

\item{add_data}{should information from a compatible df
be used to populate tree info? T/F}

\item{X}{if add_data = T, what is the predictor matrix}

\item{y}{if add_data = T  what are the true values?}

\item{model_type}{what kind of prediction you are making 'categorical' or 'regression'}
}
\description{
adapter function designed to take information from the input_tree object to the
rpart style object so that the plotting capabilities of the rpart object can be
leveraged.
}

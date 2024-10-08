% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/det.R
\name{det}
\alias{det}
\title{Density Estimation With Density Estimation Trees}
\usage{
det(
  folds = NA,
  input_model = NA,
  max_leaf_size = NA,
  min_leaf_size = NA,
  path_format = NA,
  skip_pruning = FALSE,
  test = NA,
  training = NA,
  verbose = getOption("mlpack.verbose", FALSE)
)
}
\arguments{
\item{folds}{The number of folds of cross-validation to perform for the
estimation (0 is LOOCV.  Default value "10" (integer).}

\item{input_model}{Trained density estimation tree to load (DTree).}

\item{max_leaf_size}{The maximum size of a leaf in the unpruned, fully
grown DET.  Default value "10" (integer).}

\item{min_leaf_size}{The minimum size of a leaf in the unpruned, fully
grown DET.  Default value "5" (integer).}

\item{path_format}{The format of path printing: 'lr', 'id-lr', or
'lr-id'.  Default value "lr" (character).}

\item{skip_pruning}{Whether to bypass the pruning process and output the
unpruned tree only.  Default value "FALSE" (logical).}

\item{test}{A set of test points to estimate the density of (numeric
matrix).}

\item{training}{The data set on which to build a density estimation tree
(numeric matrix).}

\item{verbose}{Display informational messages and the full list of
parameters and timers at the end of execution.  Default value
"getOption("mlpack.verbose", FALSE)" (logical).}
}
\value{
A list with several components:
\item{output_model}{Output to save trained density estimation tree to
  (DTree).}
\item{tag_counters_file}{The file to output the number of points that
  went to each leaf.  Default value "" (character).}
\item{tag_file}{The file to output the tags (and possibly paths) for
  each sample in the test set.  Default value "" (character).}
\item{test_set_estimates}{The output estimates on the test set from the
  final optimally pruned tree (numeric matrix).}
\item{training_set_estimates}{The output density estimates on the
  training set from the final optimally pruned tree (numeric matrix).}
\item{vi}{The output variable importance values for each feature
  (numeric matrix).}
}
\description{
An implementation of density estimation trees for the density estimation
task.  Density estimation trees can be trained or used to predict the density
at locations given by query points.
}
\details{
This program performs a number of functions related to Density Estimation
Trees.  The optimal Density Estimation Tree (DET) can be trained on a set of
data (specified by "training") using cross-validation (with number of folds
specified with the "folds" parameter).  This trained density estimation tree
may then be saved with the "output_model" output parameter.

The variable importances (that is, the feature importance values for each
dimension) may be saved with the "vi" output parameter, and the density
estimates for each training point may be saved with the
"training_set_estimates" output parameter.

Enabling path printing for each node outputs the path from the root node to a
leaf for each entry in the test set, or training set (if a test set is not
provided).  Strings like 'LRLRLR' (indicating that traversal went to the left
child, then the right child, then the left child, and so forth) will be
output. If 'lr-id' or 'id-lr' are given as the "path_format" parameter, then
the ID (tag) of every node along the path will be printed after or before the
L or R character indicating the direction of traversal, respectively.

This program also can provide density estimates for a set of test points,
specified in the "test" parameter.  The density estimation tree used for this
task will be the tree that was trained on the given training points, or a
tree given as the parameter "input_model".  The density estimates for the
test points may be saved using the "test_set_estimates" output parameter.
}
\author{
mlpack developers
}

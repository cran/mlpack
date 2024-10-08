% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logistic_regression.R
\name{logistic_regression}
\alias{logistic_regression}
\title{L2-regularized Logistic Regression and Prediction}
\usage{
logistic_regression(
  batch_size = NA,
  decision_boundary = NA,
  input_model = NA,
  labels = NA,
  lambda = NA,
  max_iterations = NA,
  optimizer = NA,
  print_training_accuracy = FALSE,
  step_size = NA,
  test = NA,
  tolerance = NA,
  training = NA,
  verbose = getOption("mlpack.verbose", FALSE)
)
}
\arguments{
\item{batch_size}{Batch size for SGD.  Default value "64" (integer).}

\item{decision_boundary}{Decision boundary for prediction; if the
logistic function for a point is less than the boundary, the class is taken
to be 0; otherwise, the class is 1.  Default value "0.5" (numeric).}

\item{input_model}{Existing model (parameters) (LogisticRegression).}

\item{labels}{A matrix containing labels (0 or 1) for the points in the
training set (y) (integer row).}

\item{lambda}{L2-regularization parameter for training.  Default value
"0" (numeric).}

\item{max_iterations}{Maximum iterations for optimizer (0 indicates no
limit).  Default value "10000" (integer).}

\item{optimizer}{Optimizer to use for training ('lbfgs' or 'sgd'). 
Default value "lbfgs" (character).}

\item{print_training_accuracy}{If set, then the accuracy of the model on
the training set will be printed (verbose must also be specified).  Default
value "FALSE" (logical).}

\item{step_size}{Step size for SGD optimizer.  Default value "0.01"
(numeric).}

\item{test}{Matrix containing test dataset (numeric matrix).}

\item{tolerance}{Convergence tolerance for optimizer.  Default value
"1e-10" (numeric).}

\item{training}{A matrix containing the training set (the matrix of
predictors, X) (numeric matrix).}

\item{verbose}{Display informational messages and the full list of
parameters and timers at the end of execution.  Default value
"getOption("mlpack.verbose", FALSE)" (logical).}
}
\value{
A list with several components:
\item{output_model}{Output for trained logistic regression model
  (LogisticRegression).}
\item{predictions}{If test data is specified, this matrix is where the
  predictions for the test set will be saved (integer row).}
\item{probabilities}{If test data is specified, this matrix is where the
  class probabilities for the test set will be saved (numeric matrix).}
}
\description{
An implementation of L2-regularized logistic regression for two-class
classification.  Given labeled data, a model can be trained and saved for
future use; or, a pre-trained model can be used to classify new points.
}
\details{
An implementation of L2-regularized logistic regression using either the
L-BFGS optimizer or SGD (stochastic gradient descent).  This solves the
regression problem

  y = (1 / 1 + e^-(X * b)).

In this setting, y corresponds to class labels and X corresponds to data.

This program allows loading a logistic regression model (via the
"input_model" parameter) or training a logistic regression model given
training data (specified with the "training" parameter), or both those things
at once.  In addition, this program allows classification on a test dataset
(specified with the "test" parameter) and the classification results may be
saved with the "predictions" output parameter. The trained logistic
regression model may be saved using the "output_model" output parameter.

The training data, if specified, may have class labels as its last dimension.
 Alternately, the "labels" parameter may be used to specify a separate matrix
of labels.

When a model is being trained, there are many options.  L2 regularization (to
prevent overfitting) can be specified with the "lambda" option, and the
optimizer used to train the model can be specified with the "optimizer"
parameter.  Available options are 'sgd' (stochastic gradient descent) and
'lbfgs' (the L-BFGS optimizer).  There are also various parameters for the
optimizer; the "max_iterations" parameter specifies the maximum number of
allowed iterations, and the "tolerance" parameter specifies the tolerance for
convergence.  For the SGD optimizer, the "step_size" parameter controls the
step size taken at each iteration by the optimizer.  The batch size for SGD
is controlled with the "batch_size" parameter. If the objective function for
your data is oscillating between Inf and 0, the step size is probably too
large.  There are more parameters for the optimizers, but the C++ interface
must be used to access these.

For SGD, an iteration refers to a single point. So to take a single pass over
the dataset with SGD, "max_iterations" should be set to the number of points
in the dataset.

Optionally, the model can be used to predict the responses for another matrix
of data points, if "test" is specified.  The "test" parameter can be
specified without the "training" parameter, so long as an existing logistic
regression model is given with the "input_model" parameter.  The output
predictions from the logistic regression model may be saved with the
"predictions" parameter.

This implementation of logistic regression does not support the general
multi-class case but instead only the two-class case.  Any labels must be
either 0 or 1.  For more classes, see the softmax regression implementation.
}
\examples{
# As an example, to train a logistic regression model on the data '"data"'
# with labels '"labels"' with L2 regularization of 0.1, saving the model to
# '"lr_model"', the following command may be used:

\dontrun{
output <- logistic_regression(training=data, labels=labels, lambda=0.1,
  print_training_accuracy=TRUE)
lr_model <- output$output_model
}

# Then, to use that model to predict classes for the dataset '"test"',
# storing the output predictions in '"predictions"', the following command
# may be used: 

\dontrun{
output <- logistic_regression(input_model=lr_model, test=test)
predictions <- output$predictions
}
}
\author{
mlpack developers
}

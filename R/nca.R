#' @title Neighborhood Components Analysis (NCA)
#'
#' @description
#' An implementation of neighborhood components analysis, a distance learning
#' technique that can be used for preprocessing.  Given a labeled dataset, this
#' uses NCA, which seeks to improve the k-nearest-neighbor classification, and
#' returns the learned distance metric.
#'
#' @param input Input dataset to run NCA on (numeric matrix).
#' @param armijo_constant Armijo constant for L-BFGS.  Default value
#'   "0.0001" (numeric).
#' @param batch_size Batch size for mini-batch SGD.  Default value "50"
#'   (integer).
#' @param labels Labels for input dataset (integer row).
#' @param linear_scan Don't shuffle the order in which data points are
#'   visited for SGD or mini-batch SGD.  Default value "FALSE" (logical).
#' @param max_iterations Maximum number of iterations for SGD or L-BFGS (0
#'   indicates no limit).  Default value "500000" (integer).
#' @param max_line_search_trials Maximum number of line search trials for
#'   L-BFGS.  Default value "50" (integer).
#' @param max_step Maximum step of line search for L-BFGS.  Default value
#'   "1e+20" (numeric).
#' @param min_step Minimum step of line search for L-BFGS.  Default value
#'   "1e-20" (numeric).
#' @param normalize Use a normalized starting point for optimization. This
#'   is useful for when points are far apart, or when SGD is returning NaN. 
#'   Default value "FALSE" (logical).
#' @param num_basis Number of memory points to be stored for L-BFGS. 
#'   Default value "5" (integer).
#' @param optimizer Optimizer to use; 'sgd' or 'lbfgs'.  Default value
#'   "sgd" (character).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param step_size Step size for stochastic gradient descent (alpha). 
#'   Default value "0.01" (numeric).
#' @param tolerance Maximum tolerance for termination of SGD or L-BFGS. 
#'   Default value "1e-07" (numeric).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#' @param wolfe Wolfe condition parameter for L-BFGS.  Default value "0.9"
#'   (numeric).
#'
#' @return A list with several components:
#' \item{output}{Output matrix for learned distance matrix (numeric
#'   matrix).}
#'
#' @details
#' This program implements Neighborhood Components Analysis, both a linear
#' dimensionality reduction technique and a distance learning technique.  The
#' method seeks to improve k-nearest-neighbor classification on a dataset by
#' scaling the dimensions.  The method is nonparametric, and does not require a
#' value of k.  It works by using stochastic ("soft") neighbor assignments and
#' using optimization techniques over the gradient of the accuracy of the
#' neighbor assignments.
#' 
#' To work, this algorithm needs labeled data.  It can be given as the last row
#' of the input dataset (specified with "input"), or alternatively as a separate
#' matrix (specified with "labels").
#' 
#' This implementation of NCA uses stochastic gradient descent, mini-batch
#' stochastic gradient descent, or the L_BFGS optimizer.  These optimizers do
#' not guarantee global convergence for a nonconvex objective function (NCA's
#' objective function is nonconvex), so the final results could depend on the
#' random seed or other optimizer parameters.
#' 
#' Stochastic gradient descent, specified by the value 'sgd' for the parameter
#' "optimizer", depends primarily on three parameters: the step size (specified
#' with "step_size"), the batch size (specified with "batch_size"), and the
#' maximum number of iterations (specified with "max_iterations").  In addition,
#' a normalized starting point can be used by specifying the "normalize"
#' parameter, which is necessary if many warnings of the form 'Denominator of
#' p_i is 0!' are given.  Tuning the step size can be a tedious affair.  In
#' general, the step size is too large if the objective is not mostly uniformly
#' decreasing, or if zero-valued denominator warnings are being issued.  The
#' step size is too small if the objective is changing very slowly.  Setting the
#' termination condition can be done easily once a good step size parameter is
#' found; either increase the maximum iterations to a large number and allow SGD
#' to find a minimum, or set the maximum iterations to 0 (allowing infinite
#' iterations) and set the tolerance (specified by "tolerance") to define the
#' maximum allowed difference between objectives for SGD to terminate.  Be
#' careful---setting the tolerance instead of the maximum iterations can take a
#' very long time and may actually never converge due to the properties of the
#' SGD optimizer. Note that a single iteration of SGD refers to a single point,
#' so to take a single pass over the dataset, set the value of the
#' "max_iterations" parameter equal to the number of points in the dataset.
#' 
#' The L-BFGS optimizer, specified by the value 'lbfgs' for the parameter
#' "optimizer", uses a back-tracking line search algorithm to minimize a
#' function.  The following parameters are used by L-BFGS: "num_basis"
#' (specifies the number of memory points used by L-BFGS), "max_iterations",
#' "armijo_constant", "wolfe", "tolerance" (the optimization is terminated when
#' the gradient norm is below this value), "max_line_search_trials", "min_step",
#' and "max_step" (which both refer to the line search routine).  For more
#' details on the L-BFGS optimizer, consult either the mlpack L-BFGS
#' documentation (in lbfgs.hpp) or the vast set of published literature on
#' L-BFGS.
#' 
#' By default, the SGD optimizer is used.
#'
#' @author
#' mlpack developers
#'
#' @export
nca <- function(input,
                armijo_constant=NA,
                batch_size=NA,
                labels=NA,
                linear_scan=FALSE,
                max_iterations=NA,
                max_line_search_trials=NA,
                max_step=NA,
                min_step=NA,
                normalize=FALSE,
                num_basis=NA,
                optimizer=NA,
                seed=NA,
                step_size=NA,
                tolerance=NA,
                verbose=getOption("mlpack.verbose", FALSE),
                wolfe=NA) {
  # Create parameters and timers objects.
  p <- CreateParams("nca")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  if (!identical(armijo_constant, NA)) {
    SetParamDouble(p, "armijo_constant", armijo_constant)
  }

  if (!identical(batch_size, NA)) {
    SetParamInt(p, "batch_size", batch_size)
  }

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  if (!identical(linear_scan, FALSE)) {
    SetParamBool(p, "linear_scan", linear_scan)
  }

  if (!identical(max_iterations, NA)) {
    SetParamInt(p, "max_iterations", max_iterations)
  }

  if (!identical(max_line_search_trials, NA)) {
    SetParamInt(p, "max_line_search_trials", max_line_search_trials)
  }

  if (!identical(max_step, NA)) {
    SetParamDouble(p, "max_step", max_step)
  }

  if (!identical(min_step, NA)) {
    SetParamDouble(p, "min_step", min_step)
  }

  if (!identical(normalize, FALSE)) {
    SetParamBool(p, "normalize", normalize)
  }

  if (!identical(num_basis, NA)) {
    SetParamInt(p, "num_basis", num_basis)
  }

  if (!identical(optimizer, NA)) {
    SetParamString(p, "optimizer", optimizer)
  }

  if (!identical(seed, NA)) {
    SetParamInt(p, "seed", seed)
  }

  if (!identical(step_size, NA)) {
    SetParamDouble(p, "step_size", step_size)
  }

  if (!identical(tolerance, NA)) {
    SetParamDouble(p, "tolerance", tolerance)
  }

  if (!identical(verbose, FALSE)) {
    SetParamBool(p, "verbose", verbose)
  }

  if (!identical(wolfe, NA)) {
    SetParamDouble(p, "wolfe", wolfe)
  }

  # Mark all output options as passed.
  SetPassed(p, "output")

  # Call the program.
  nca_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = GetParamMat(p, "output")
  )


  return(out)
}

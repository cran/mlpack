#' @title K-Rank-Approximate-Nearest-Neighbors (kRANN)
#'
#' @description
#' An implementation of rank-approximate k-nearest-neighbor search (kRANN) 
#' using single-tree and dual-tree algorithms.  Given a set of reference points
#' and query points, this can find the k nearest neighbors in the reference set
#' of each query point using trees; trees that are built can be saved for future
#' use.
#'
#' @param alpha The desired success probability.  Default value "0.95"
#'   (numeric).
#' @param first_leaf_exact The flag to trigger sampling only after exactly
#'   exploring the first leaf.  Default value "FALSE" (logical).
#' @param input_model Pre-trained kNN model (RAModel).
#' @param k Number of nearest neighbors to find.  Default value "0"
#'   (integer).
#' @param leaf_size Leaf size for tree building (used for kd-trees, UB
#'   trees, R trees, R* trees, X trees, Hilbert R trees, R+ trees, R++ trees,
#'   and octrees).  Default value "20" (integer).
#' @param naive If true, sampling will be done without using a tree. 
#'   Default value "FALSE" (logical).
#' @param query Matrix containing query points (optional) (numeric
#'   matrix).
#' @param random_basis Before tree-building, project the data onto a random
#'   orthogonal basis.  Default value "FALSE" (logical).
#' @param reference Matrix containing the reference dataset (numeric
#'   matrix).
#' @param sample_at_leaves The flag to trigger sampling at leaves.  Default
#'   value "FALSE" (logical).
#' @param seed Random seed (if 0, std::time(NULL) is used).  Default value
#'   "0" (integer).
#' @param single_mode If true, single-tree search is used (as opposed to
#'   dual-tree search.  Default value "FALSE" (logical).
#' @param single_sample_limit The limit on the maximum number of samples
#'   (and hence the largest node you can approximate).  Default value "20"
#'   (integer).
#' @param tau The allowed rank-error in terms of the percentile of the
#'   data.  Default value "5" (numeric).
#' @param tree_type Type of tree to use: 'kd', 'ub', 'cover', 'r', 'x',
#'   'r-star', 'hilbert-r', 'r-plus', 'r-plus-plus', 'oct'.  Default value "kd"
#'   (character).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components:
#' \item{distances}{Matrix to output distances into (numeric matrix).}
#' \item{neighbors}{Matrix to output neighbors into (integer matrix).}
#' \item{output_model}{If specified, the kNN model will be output here
#'   (RAModel).}
#'
#' @details
#' This program will calculate the k rank-approximate-nearest-neighbors of a set
#' of points. You may specify a separate set of reference points and query
#' points, or just a reference set which will be used as both the reference and
#' query set. You must specify the rank approximation (in %) (and optionally the
#' success probability).
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, the following will return 5 neighbors from the top 0.1% of the
#' # data (with probability 0.95) for each point in "input" and store the
#' # distances in "distances" and the neighbors in "neighbors.csv":
#' 
#' \dontrun{
#' output <- krann(reference=input, k=5, tau=0.1)
#' distances <- output$distances
#' neighbors <- output$neighbors
#' }
#' 
#' # Note that tau must be set such that the number of points in the
#' # corresponding percentile of the data is greater than k.  Thus, if we choose
#' # tau = 0.1 with a dataset of 1000 points and k = 5, then we are attempting
#' # to choose 5 nearest neighbors out of the closest 1 point -- this is invalid
#' # and the program will terminate with an error message.
#' # 
#' # The output matrices are organized such that row i and column j in the
#' # neighbors output file corresponds to the index of the point in the
#' # reference set which is the i'th nearest neighbor from the point in the
#' # query set with index j.  Row i and column j in the distances output file
#' # corresponds to the distance between those two points.
krann <- function(alpha=NA,
                  first_leaf_exact=FALSE,
                  input_model=NA,
                  k=NA,
                  leaf_size=NA,
                  naive=FALSE,
                  query=NA,
                  random_basis=FALSE,
                  reference=NA,
                  sample_at_leaves=FALSE,
                  seed=NA,
                  single_mode=FALSE,
                  single_sample_limit=NA,
                  tau=NA,
                  tree_type=NA,
                  verbose=getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("krann")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  if (!identical(alpha, NA)) {
    SetParamDouble(p, "alpha", alpha)
  }

  if (!identical(first_leaf_exact, FALSE)) {
    SetParamBool(p, "first_leaf_exact", first_leaf_exact)
  }

  if (!identical(input_model, NA)) {
    SetParamRAModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  if (!identical(k, NA)) {
    SetParamInt(p, "k", k)
  }

  if (!identical(leaf_size, NA)) {
    SetParamInt(p, "leaf_size", leaf_size)
  }

  if (!identical(naive, FALSE)) {
    SetParamBool(p, "naive", naive)
  }

  if (!identical(query, NA)) {
    SetParamMat(p, "query", to_matrix(query), TRUE)
  }

  if (!identical(random_basis, FALSE)) {
    SetParamBool(p, "random_basis", random_basis)
  }

  if (!identical(reference, NA)) {
    SetParamMat(p, "reference", to_matrix(reference), TRUE)
  }

  if (!identical(sample_at_leaves, FALSE)) {
    SetParamBool(p, "sample_at_leaves", sample_at_leaves)
  }

  if (!identical(seed, NA)) {
    SetParamInt(p, "seed", seed)
  }

  if (!identical(single_mode, FALSE)) {
    SetParamBool(p, "single_mode", single_mode)
  }

  if (!identical(single_sample_limit, NA)) {
    SetParamInt(p, "single_sample_limit", single_sample_limit)
  }

  if (!identical(tau, NA)) {
    SetParamDouble(p, "tau", tau)
  }

  if (!identical(tree_type, NA)) {
    SetParamString(p, "tree_type", tree_type)
  }

  if (!identical(verbose, FALSE)) {
    SetParamBool(p, "verbose", verbose)
  }

  # Mark all output options as passed.
  SetPassed(p, "distances")
  SetPassed(p, "neighbors")
  SetPassed(p, "output_model")

  # Call the program.
  krann_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamRAModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "RAModel"

  # Extract the results in order.
  out <- list(
      "distances" = GetParamMat(p, "distances"),
      "neighbors" = GetParamUMat(p, "neighbors"),
      "output_model" = output_model
  )


  return(out)
}

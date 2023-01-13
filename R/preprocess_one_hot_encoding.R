#' @title One Hot Encoding
#'
#' @description
#' A utility to do one-hot encoding on features of dataset.
#'
#' @param dimensions Index of dimensions thatneed to be one-hot encoded
#'   (integer vector).
#' @param input Matrix containing data (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{output}{Matrix to save one-hot encoded features data to (numeric
#'   matrix).}
#'
#' @details
#' This utility takes a dataset and a vector of indices and does one-hot
#' encoding of the respective features at those indices. Indices represent the
#' IDs of the dimensions to be one-hot encoded.
#' 
#' The output matrix with encoded features may be saved with the "output"
#' parameters.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # So, a simple example where we want to encode 1st and 3rd feature from
#' # dataset "X" into "X_output" would be
#' 
#' \dontrun{
#' output <- preprocess_one_hot_encoding(input=X, dimensions=1, dimensions=3)
#' X_ouput <- output$output
#' }
preprocess_one_hot_encoding <- function(dimensions,
                                        input,
                                        verbose=FALSE) {
  # Create parameters and timers objects.
  p <- CreateParams("preprocess_one_hot_encoding")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamVecInt(p, "dimensions", dimensions)

  SetParamMat(p, "input", to_matrix(input), TRUE)

  if (verbose) {
    EnableVerbose()
  } else {
    DisableVerbose()
  }

  # Mark all output options as passed.
  SetPassed(p, "output")

  # Call the program.
  preprocess_one_hot_encoding_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = GetParamMat(p, "output")
  )


  return(out)
}

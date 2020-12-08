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
  # Restore IO settings.
  IO_RestoreSettings("One Hot Encoding")

  # Process each input argument before calling mlpackMain().
  IO_SetParamVecInt("dimensions", dimensions)

  IO_SetParamMat("input", to_matrix(input))

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")

  # Call the program.
  preprocess_one_hot_encoding_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamMat("output")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}

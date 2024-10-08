% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kfn.R
\name{kfn}
\alias{kfn}
\title{k-Furthest-Neighbors Search}
\usage{
kfn(
  algorithm = NA,
  epsilon = NA,
  input_model = NA,
  k = NA,
  leaf_size = NA,
  percentage = NA,
  query = NA,
  random_basis = FALSE,
  reference = NA,
  seed = NA,
  tree_type = NA,
  true_distances = NA,
  true_neighbors = NA,
  verbose = getOption("mlpack.verbose", FALSE)
)
}
\arguments{
\item{algorithm}{Type of neighbor search: 'naive', 'single_tree',
'dual_tree', 'greedy'.  Default value "dual_tree" (character).}

\item{epsilon}{If specified, will do approximate furthest neighbor
search with given relative error. Must be in the range [0,1).  Default
value "0" (numeric).}

\item{input_model}{Pre-trained kFN model (KFNModel).}

\item{k}{Number of furthest neighbors to find.  Default value "0"
(integer).}

\item{leaf_size}{Leaf size for tree building (used for kd-trees, vp
trees, random projection trees, UB trees, R trees, R* trees, X trees,
Hilbert R trees, R+ trees, R++ trees, and octrees).  Default value "20"
(integer).}

\item{percentage}{If specified, will do approximate furthest neighbor
search. Must be in the range (0,1] (decimal form). Resultant neighbors will
be at least (p*100) % of the distance as the true furthest neighbor. 
Default value "1" (numeric).}

\item{query}{Matrix containing query points (optional) (numeric
matrix).}

\item{random_basis}{Before tree-building, project the data onto a random
orthogonal basis.  Default value "FALSE" (logical).}

\item{reference}{Matrix containing the reference dataset (numeric
matrix).}

\item{seed}{Random seed (if 0, std::time(NULL) is used).  Default value
"0" (integer).}

\item{tree_type}{Type of tree to use: 'kd', 'vp', 'rp', 'max-rp', 'ub',
'cover', 'r', 'r-star', 'x', 'ball', 'hilbert-r', 'r-plus', 'r-plus-plus',
'oct'.  Default value "kd" (character).}

\item{true_distances}{Matrix of true distances to compute the effective
error (average relative error) (it is printed when -v is specified)
(numeric matrix).}

\item{true_neighbors}{Matrix of true neighbors to compute the recall (it
is printed when -v is specified) (integer matrix).}

\item{verbose}{Display informational messages and the full list of
parameters and timers at the end of execution.  Default value
"getOption("mlpack.verbose", FALSE)" (logical).}
}
\value{
A list with several components:
\item{distances}{Matrix to output distances into (numeric matrix).}
\item{neighbors}{Matrix to output neighbors into (integer matrix).}
\item{output_model}{If specified, the kFN model will be output here
  (KFNModel).}
}
\description{
An implementation of k-furthest-neighbor search using single-tree and
dual-tree algorithms.  Given a set of reference points and query points, this
can find the k furthest neighbors in the reference set of each query point
using trees; trees that are built can be saved for future use.
}
\details{
This program will calculate the k-furthest-neighbors of a set of points. You
may specify a separate set of reference points and query points, or just a
reference set which will be used as both the reference and query set.
}
\examples{
# For example, the following will calculate the 5 furthest neighbors of
# eachpoint in "input" and store the distances in "distances" and the
# neighbors in "neighbors": 

\dontrun{
output <- kfn(k=5, reference=input)
distances <- output$distances
neighbors <- output$neighbors
}

# The output files are organized such that row i and column j in the
# neighbors output matrix corresponds to the index of the point in the
# reference set which is the j'th furthest neighbor from the point in the
# query set with index i.  Row i and column j in the distances output file
# corresponds to the distance between those two points.
}
\author{
mlpack developers
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/serialization.R
\name{Serialize}
\alias{Serialize}
\alias{Unserialize}
\title{Serialize/Unserialize an mlpack model.}
\usage{
Serialize(model, filename)

Unserialize(filename)
}
\arguments{
\item{model}{Input model pointer.}

\item{filename}{Input filename.}
}
\value{
For Unserialize, Output model_ptr.
}
\description{
Serialize/Unserialize an mlpack model.
}

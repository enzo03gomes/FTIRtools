% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SGfilter.R
\name{SG.parameters}
\alias{SG.parameters}
\title{Calculate the matrix of paramaters for Savitsky-Golay Filter}
\usage{
SG.parameters(N, M)
}
\arguments{
\item{N}{polynomial order}

\item{M}{window half-width}
}
\value{
the pseudo-inverse of the Valdemonde matrix
}
\description{
Calculate the matrix of paramaters for Savitsky-Golay Filter
}

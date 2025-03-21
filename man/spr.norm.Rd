% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalization.R
\name{spr.norm}
\alias{spr.norm}
\title{Normalize FTIR spectra}
\usage{
spr.norm(spr, type = "L-norm", order = 2, target = 1)
}
\arguments{
\item{spr}{object of class spectra}

\item{type}{normalization type. Options are "L-norm", "area", "max" and "SNV"}

\item{order}{Order of L-norm. Used only if type == "L-norm"}

\item{target}{If type == "max", target is the new maximum value; if type == "SNV", target is the new total area}
}
\description{
Normalize FTIR spectra
}

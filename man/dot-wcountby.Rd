% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.r
\name{.wcountby}
\alias{.wcountby}
\title{Tabulation utility for data table (internal)}
\usage{
.wcountby(DT, .x, bycols, .x_excludena, bycols_excludena, .w, .f, side_effects)
}
\arguments{
\item{.x}{character vector containing columns to be tabulated}

\item{bycols}{a character vector containing columns by which to group data}

\item{.x_excludena}{one of NULL, a boolean, or a character vector -- see
description}

\item{bycols_excludena}{one of NULL, a boolean, or a character vector -- see
description}

\item{.w}{a character vector specifying the weight column}

\item{.f}{the counting function acting on either a vector of ones when
weights are not supplied, or acting on a vector of ones and weights when
weights are supplied.}

\item{side_effects}{whether we should alter \code{DT} in-place (\code{side_effects = TRUE}) or make a copy (\code{side_effects = FALSE})}
}
\value{
a named list where entries correspond to variables in \code{.x},
containing their tabulation
}
\description{
This function provides a count of columns in \code{.x} by groups defined by
columns \code{bycols}. The count function is specified as .f, and it is supposed
to act on vectors of ones for each unique value of \code{.x[i]} in groups.
}
\details{
If weights are provided, \code{.f} should accept two arguments, one of which is a
vector of ones, and the other is a vector of weights.
}

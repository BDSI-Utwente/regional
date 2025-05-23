% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disproportionality.R
\name{get_disproportionality_indices}
\alias{get_disproportionality_indices}
\title{Get indices of disproportionality}
\usage{
get_disproportionality_indices(votes, seats)
}
\arguments{
\item{votes}{vector of votes for each party}

\item{seats}{vector of seats allocated to each party}
}
\value{
vector with named elements sli, gi, lhi, and diff, corresponding to
the Saint-Langue, Gallagher, Loosemore-Hanby and absolute seat difference
indices,  respectively.
}
\description{
Given two vectors of vote counts and allocated seats, calculate the Loosemore-Hanby,
Gallagher, and Saint-Langue indices of disproportionality.
}
\details{
This is simply a wrapper around the individual indices to simplify calculation
of multiple indices at once.

Note that this function assumes usage in a given context, and is likely of limited use
outside of that context. Consider simply calling the indices you need individually.
}

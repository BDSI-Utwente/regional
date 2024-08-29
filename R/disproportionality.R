#' Saint-Langue index of disproportionality
#'
#' Given two vectors of vote counts and allocated seats, calculate the Saint-Langue
#' index of disproportionality. This index is minimized by largest averages seat
#' allocation methods, e.g. d'Hondt or (unsurprisingly) Saint-Langue.
#'
#' A Saint-Langue index of zero corresponds to perfectly proportional seat allocation,
#' whereas a high index indicates greater disproportionality. The Saint-Langue index
#' is theoretically infinite if any party is allocated a seat without receiving
#' any votes.
#'
#' @param votes vector of votes for each party
#' @param seats vector of seats allocated to each party
#'
#' @return a value between 0 and infinity.
#' @export
saint_lague_index <- function(votes, seats) {
  vote_shares <- votes / sum(votes) * 100
  seat_shares <- seats / sum(seats) * 100
  sum((seat_shares - vote_shares)^2 / vote_shares)
}

#' Gallagher index of disproportionality
#'
#' Given two vectors of vote counts and allocated seats, calculate the Gallagher
#' (or Least Squares) index of disproportionality. This index takes a middle ground
#' between the Saint-Langue and Loosemore Hanby indices, that are minimized by
#' largest averages and largest remainder seat allocation methods, respectively.
#'
#' A Gallagher index of zero corresponds to perfectly proportional seat allocation,
#' whereas a high index indicates greater disproportionality. The Gallagher index
#' ranges from 0 to 100.
#'
#' @param votes vector of votes for each party
#' @param seats vector of seats allocated to each party
#'
#' @return a value between 0 and 100
#' @export
gallagher_index <- function(votes, seats) {
  vote_shares <- votes / sum(votes) * 100
  seat_shares <- seats / sum(seats) * 100
  sqrt(sum((seat_shares - vote_shares)^2)/2)
}

#' Loosemore-Hanby index of disproportionality
#'
#' Given two vectors of vote counts and allocated seats, calculate the Loosemore-Hanby
#' index of disproportionality. This index is minimized by largest remainder seat
#' allocation methods, e.g. Hare.
#'
#' A Loosemore-Hanby index of zero corresponds to perfectly proportional seat allocation,
#' whereas a high index indicates greater disproportionality. The Loosemore-Hanby
#' index ranges from 0 to 1.
#'
#' @param votes vector of votes for each party
#' @param seats vector of seats allocated to each party
#'
#' @return a value between 0 and 1.
#' @export
loosemore_hanby_index <- function(votes, seats) {
  vote_shares <- votes / sum(votes)
  seat_shares <- seats / sum(seats)
  sum(abs(seat_shares - vote_shares)) / 2
}


#' Get indices of disproportionality
#'
#' Given two vectors of vote counts and allocated seats, calculate the Loosemore-Hanby,
#' Gallagher, and Saint-Langue indices of disproportionality.
#'
#' This is simply a wrapper around the individual indices to simplify calculation
#' of multiple indices at once.
#'
#' @param votes vector of votes for each party
#' @param seats vector of seats allocated to each party
#'
#' @return vector with named elements sli, gi, and lhi, corresponding to the Saint-Langue,
#'  Gallagher, and Loosemore-Hanby indices respectively.
#' @export
get_disproportionality_indeces <- function(votes, seats) {
  c(sli = saint_lague_index(votes, seats),
    gi = gallagher_index(votes, seats),
    lhi = loosemore_hanby_index(votes, seats))
}

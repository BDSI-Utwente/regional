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
#' @examples
#' # Saint-Langue index for a mostly proportional outcome
#' votes <- c(0.4, 0.3, 0.2, 0.06, 0.03, 0.01) * 1e6
#' parties <- paste("Party", letters[1:6])
#' outcome <- allocate_seats(parties, votes, 120)
#' saint_langue_index(votes, outcome$seats)
#'
#' # Using Hare allocation
#' outcome_hare <- allocate_seats(parties, votes, 120, method = "hare")
#' saint_langue_index(votes, outcome_hare$seats)
#'
#' # Saint-Langue is (over)sensitive to small parties gaining fixed seats
#' # assigning a fixed seat to a tiny party (e.g. to ensure minority representation)
#' votes <- c(0.409, 0.3, 0.2, 0.06, 0.03, 0.001) * 1e6
#' outcome <- allocate_seats(parties, votes, 120, fixed_seats = c(rep(0,5), 1))
#' saint_langue_index(votes, outcome$seats)
#'
saint_langue_index <- function(votes, seats) {
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
#' @examples
#' # Gallagher index for a mostly proportional outcome
#' votes <- c(0.4, 0.3, 0.2, 0.06, 0.03, 0.01) * 1e6
#' outcome <- allocate_seats(paste("Party", letters[1:6]), votes, 120)
#' gallagher_index(votes, outcome$seats)
#'
#' # Using Hare allocation
#' outcome_hare <- allocate_seats(paste("Party", letters[1:6]), votes, 120, method = "hare")
#' gallagher_index(votes, outcome_hare$seats)
#'
gallagher_index <- function(votes, seats) {
  vote_shares <- votes / sum(votes) * 100
  seat_shares <- seats / sum(seats) * 100
  sqrt(sum((seat_shares - vote_shares)^2) / 2)
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
#' @examples
#' # Loosemore-Hanby index for a mostly proportional outcome
#' votes <- c(0.4, 0.3, 0.2, 0.06, 0.03, 0.01) * 1e6
#' parties <- paste("Party", letters[1:6])
#' outcome <- allocate_seats(parties, votes, 120)
#' loosemore_hanby_index(votes, outcome$seats)
#'
#' # Using Hare allocation
#' outcome_hare <- allocate_seats(parties, votes, 120, method = "hare")
#' loosemore_hanby_index(votes, outcome_hare$seats)
#'
loosemore_hanby_index <- function(votes, seats) {
  vote_shares <- votes / sum(votes)
  seat_shares <- seats / sum(seats)
  sum(abs(seat_shares - vote_shares)) / 2
}


#' Absolute seat difference
#'
#' Absolute difference in seats from a known baseline seats allocation method.
#'
#' Calculates seat allocation under the baseline, then returns the sum of absolute
#' seat differences. The sum is divided by two, as each seat gained by one party,
#' is always a seat lost by another party.
#'
#' @param votes vector of votes
#' @param seats vector of allocated seats
#' @param baseline_seats_fn function to calculate baseline seat allocation to
#' compare with. Should take a vector of vote counts as the sole argument. The
#' default, a thin wrapper around `allocate_seats_nl`, follows seat allocation
#' for the Dutch Parliament.
#'
#' @return integer sum of absolute seat differences
#' @export
#' @examples
#' # Compare absolute seat difference between d'Hondt and Hare allocation
#' votes <- c(0.4, 0.3, 0.2, 0.06, 0.03, 0.01) * 1e6
#' parties <- paste("Party", letters[1:6])
#' outcome <- allocate_seats(parties, votes, 120)
#'
#' # seat_difference takes a baseline function with a vote vector as its only
#' # argument, let's creata a thin wrapper that sets the other arguments of
#' # allocate_seats appropriately.
#' baseline_fn <- \(votes) allocate_seats(1:length(votes), votes, 120, method = "hare")$seats
#' seat_difference(votes, outcome$seats, baseline_fn)
seat_difference <- function(
  votes,
  seats,
  baseline_seats_fn = \(votes) allocate_seats_nl(1:length(votes), votes)$seats
) {
  baseline_seats <- baseline_seats_fn(votes)
  diff <- (baseline_seats - seats[order(-votes, -seats)])
  sum(abs(diff)) / 2
}


#' Get indices of disproportionality
#'
#' Given two vectors of vote counts and allocated seats, calculate the Loosemore-Hanby,
#' Gallagher, and Saint-Langue indices of disproportionality.
#'
#' This is simply a wrapper around the individual indices to simplify calculation
#' of multiple indices at once.
#'
#' Note that this function assumes usage in a given context, and is likely of limited use
#' outside of that context. Consider simply calling the indices you need individually.
#'
#' @param votes vector of votes for each party
#' @param seats vector of seats allocated to each party
#'
#' @return vector with named elements sli, gi, lhi, and diff, corresponding to
#' the Saint-Langue, Gallagher, Loosemore-Hanby and absolute seat difference
#' indices,  respectively.
#' @export
get_disproportionality_indices <- function(votes, seats) {
  c(
    sli = saint_langue_index(votes, seats),
    gi = gallagher_index(votes, seats),
    lhi = loosemore_hanby_index(votes, seats),
    diff = seat_difference(votes, seats)
  )
}

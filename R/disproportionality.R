saint_lague_index <- function(votes, seats) {
  vote_shares <- votes / sum(votes) * 100
  seat_shares <- seats / sum(seats) * 100
  sum((seat_shares - vote_shares)^2 / vote_shares)
}

gallagher_index <- function(votes, seats) {
  vote_shares <- votes / sum(votes) * 100
  seat_shares <- seats / sum(seats) * 100
  sqrt(sum((seat_shares - vote_shares)^2)/2)
}

loosemore_hanby_index <- function(votes, seats) {
  vote_shares <- votes / sum(votes)
  seat_shares <- seats / sum(seats)
  sum(abs(seat_shares - vote_shares)) / 2
}

get_disproportionality_indeces <- function(votes, seats) {
  c(sli = saint_lague_index(votes, seats),
    gi = gallagher_index(votes, seats),
    lhi = loosemore_hanby_index(votes, seats))
}

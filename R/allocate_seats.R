LARGEST_REMAINDER_METHODS <- c("hare")
LARGEST_AVERAGES_METHODS <- c("dh")

#' Allocate seats across parties using different methods of allocation
#'
#' Given a set of parties and associated vote counts, allocate `seats` number of
#' seats using one of several common methods of seat allocation.
#'
#' @param parties vector of party names (or other identifiers)
#' @param votes vector of vote counts per party
#' @param seats total number of seats available, **including** any seats already
#'  allocated using the `fixed_seats` argument.
#' @param fixed_seats vector of seat counts already allocated to each party that
#'  should be considered 'fixed'. Defaults to a zero vector, i.e., no seats have
#'  yet been allocated.
#' @param method one of several methods of seat allocation. See details. Defaults
#'  to 'dh' for d'Hondt method.
#' @param threshold vote share required to be eligible for any seats. Defaults to
#'  0, should be in the range [0,1]. Common values are between 0 and 0.05.
#' @param fixed_seats_override_threshold should parties with fixed seats automatically
#'  be eligible for remainder seat allocation? This is common in some parliamentary
#'  systems. Defaults to `FALSE`.
#'
#' @return A tibble containing several columns:
#' \item{party}{party name}
#' \item{votes}{number of votes}
#' \item{seats}{total number of assigned seats}
#' \item{fixed_seats}{number of previously allocated seats considered 'fixed'}
#' \item{allocated_seats}{number of seats allocated in this allocation step,
#'  equal to `seats - fixed_seats`}
#' \item{ideal_seats}{'ideal' number of seats under perfectly proportional
#'  representation. Calculated as `votes / sum(votes) * seats`}
#' @export
allocate_seats <- function(parties,
                           votes,
                           seats,
                           fixed_seats = rep(0, length(parties)),
                           method = c("dh", "hare"),
                           threshold = 0,
                           fixed_seat_overrides_threshold = FALSE) {

  if (length(method) > 1) {
    method <- method[1]
  }


  if (method %in% LARGEST_REMAINDER_METHODS) {
    .allocate_seats_lr(
      parties,
      votes,
      seats,
      fixed_seats,
      method,
      threshold,
      fixed_seat_overrides_threshold
    ) |>
      arrange(desc(votes), desc(seats))
  } else if (method %in% LARGEST_AVERAGES_METHODS) {
    .allocate_seats_la(
      parties,
      votes,
      seats,
      fixed_seats,
      method,
      threshold,
      fixed_seat_overrides_threshold
    ) |>
      arrange(desc(votes), desc(seats))
  } else {
    stop(glue::glue("unknown allocation method: {method}"))
  }
}

.allocate_seats_lr <- function(parties,
                               votes,
                               seats,
                               fixed_seats,
                               method,
                               threshold,
                               fixed_seat_overrides_threshold) {

  # calculate quota depending on allocation method (only hare is implemented for now)
  if (method == "hare") {
    quota = sum(votes) / seats
  } else {
    stop(glue::glue("allocation method not implemented: {method}"))
  }

  # vote counts required to fill quota for 1:n seats
  quota_votes = (1:seats) * quota

  # votes required to meet threshold
  threshold_votes = sum(votes) * threshold

  # create party votes data structure
  party_seats <- tibble(party = parties, votes, fixed_seats) %>%
    mutate(ideal_seats = votes / sum(votes) * seats,
           eligible = votes > threshold_votes | (fixed_seats > 0 & fixed_seat_overrides_threshold))

  # expand grid of party/quota needed for 1:n seats
  expand_grid(party = parties, quota_votes) %>%
    right_join(party_seats, by = join_by(party)) %>%
    mutate(
      # add rank-order for potential seats, lowest quota_votes (used) first
      rank = rank(quota_votes), .by = party,

      # any potential seat with a rank <= fixed seats is already assigned
      seat_is_fixed = rank <= fixed_seats) %>%

    # potential seats must be eligible (i.e., meet threshold) or fixed
    filter(eligible | seat_is_fixed) %>%

    # sort potential seats by fixed status, then remaining votes quota, and last
    # by a random component to settle ties
    arrange(desc(seat_is_fixed), desc(votes - quota_votes), rnorm(n())) %>%

    # select top n rows, where n is the number of seats
    slice_head(n = seats) %>%

    # summarize as seats per party
    count(party, name = "seats") %>%

    # add back in party-level data we started with, and fill in seats = 0 for
    # parties that did not get any seats
    right_join(party_seats, by = join_by(party)) %>%
    replace_na(list(seats = 0)) %>%

    # some final cleanup...
    mutate(allocated_seats = seats - fixed_seats) %>%
    relocate(party, votes, seats, fixed_seats, allocated_seats, ideal_seats)
}

.allocate_seats_la <- function(parties,
                               votes,
                               seats,
                               fixed_seats,
                               method,
                               threshold,
                               fixed_seat_overrides_threshold) {
  # calculate quota depending on allocation method (only dHondt is implemented for now)
  if (method == "dh") {
    divisor = 1:seats
  } else {
    stop(glue::glue("allocation method not implemented: {method}"))
  }


  # votes required to meet threshold
  threshold_votes = sum(votes) * threshold

  # create party data
  party_seats <- tibble(party = parties, votes, fixed_seats) %>%
    mutate(ideal_seats = votes / sum(votes) * seats,
           eligible = votes > threshold_votes | (fixed_seats > 0 & fixed_seat_overrides_threshold))

  # create quotients list
  quotients <- expand_grid(party = parties, divisor) %>%
    left_join(party_seats, by = join_by(party)) %>%
    mutate(quotient = votes / divisor) %>%
    mutate(quotient_rank = rank(-quotient), .by = party) %>%

    # mark seats that were already allocated (in districts) as assigned, so we
    # can include them regardless of the quotient.
    mutate(seat_is_fixed = quotient_rank <= fixed_seats)

  # order by already assigned, max quotient, and break ties with a random component
  # then take the top rows as seat winners.
  #
  # Note that ordering by assigned first makes sure that previously assigned seats
  # are included, even if they would not be allocated based on the quotient alone.
  # Including a random component solves the tie-breaking problem without having
  # to implement an additional check.
  quotients %>%
    filter(eligible | seat_is_fixed) %>%
    arrange(desc(seat_is_fixed), desc(quotient), rnorm(n())) %>%
    slice_head(n = seats) %>%
    count(party, name = "seats") %>%
    right_join(party_seats, by = join_by(party)) %>%
    replace_na(replace = list(seats = 0)) %>%
    mutate(allocated_seats = seats - fixed_seats) %>%
    relocate(party, votes, seats, fixed_seats, allocated_seats, ideal_seats)
}


#' Allocate seats per the Dutch Parliamentary electoral system.
#'
#' This is a thin wrapper around `allocate_seats(...)` using default settings as
#' used in Dutch for the Dutch house of representatives ("Tweede Kamer").
#'
#' @param parties vector of party names
#' @param votes vector of vote counts, of the same length as `parties`
#'
#' @return see `allocate_votes`
#' @export
allocate_seats_nl <- function(parties, votes) {
  allocate_seats(parties, votes, 150, threshold = 1/150, method = "dh")
}

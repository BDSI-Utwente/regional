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
#'  0, should be in the range \[0,1\]. Common values are between 0 and 0.05.
#' @param fixed_seat_overrides_threshold should parties with fixed seats automatically
#'  be eligible for remainder seat allocation? This is common in some parliamentary
#'  systems. Defaults to `FALSE`.
#' @param lr_extra_seat_resolution_method in largest remainder methods, it is 
#'  theoretically possible for there to be more remainder seats than parties. In 
#'  such cases, the remaining seats may be allocated using a different allocation
#'  method. If `FALSE`, remaining seats are given to parties with the least negative
#'  remaining votes after allocating seats. Otherwise, this should be the name of a 
#'  seat allocation method. Defaults to `dh`.
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
#' @examples
#' # Given a set of parties ...
#' parties <- paste("Party", letters[1:6])
#'
#' # ... a vector of votes cast ...
#' votes <- round(runif(6) * 1e6)
#'
#' # ... and total number of seats available ...
#' seats <- 100
#'
#' # ... allocate seats to parties proportional to their vote count
#' allocate_seats(parties, votes, seats)
#'
#' # We can also add a number of 'fixed' seats, that parties are
#' # guaranteed to get.
#' # For instance, we can ensure each party gets at least 10 seats...
#' allocate_seats(parties, votes, seats, fixed_seats = rep(10, 6))
#'
#' # We may want limit fragmentation and only assign seats to parties
#' # larger than a given threshold.
#' # Create known list of large and small parties
#' votes <- round(c(0.4, 0.3, 0.2, 0.06, 0.03, 0.01) * 1e6)
#' allocate_seats(parties, votes, seats, threshold = 0.05)
#'
#' # Or even do both at the same time...
#' allocate_seats(parties, votes, seats, threshold = 0.05, fixed_seats = rep(10, 6))
#' # note that fixed seats are NEVER taken away, regardless of the
#' # used threshold. The number of seats available to remaining parties
#' # is automatically compensated.
#'
#' # In some electoral systems, parties that gain a regional seat
#' # are exempt from a national threshold. If we give all parties a single
#' # fixed seat, we can see that Party e will be allocated 2 additional
#' # seats, even though the party does not meet the vote share threshold.
#' # Party f would now also be eligible for additional seats, but doesn't
#' # have to vote share to be allocated one.
#' allocate_seats(parties, votes, seats, threshold = 0.05, fixed_seats = rep(1, 6), fixed_seat_overrides_threshold = TRUE)
allocate_seats <- function(
  parties,
  votes,
  seats,
  fixed_seats = rep(0, length(parties)),
  method = c("dh", "hare"),
  threshold = 0,
  fixed_seat_overrides_threshold = FALSE,
  lr_extra_seat_resolution_method = "dh"
) {
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
      fixed_seat_overrides_threshold,
      lr_extra_seat_resolution_method
    ) |>
      dplyr::arrange(dplyr::desc(votes), dplyr::desc(seats))
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
      dplyr::arrange(dplyr::desc(votes), dplyr::desc(seats))
  } else {
    stop(glue::glue("unknown allocation method: {method}"))
  }
}

.allocate_seats_lr <- function(
  parties,
  votes,
  seats,
  fixed_seats,
  method,
  threshold,
  fixed_seat_overrides_threshold,
  lr_extra_seat_resolution_method
) {
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
  party_seats <- tibble::tibble(party = parties, votes, fixed_seats) %>%
    dplyr::mutate(
      ideal_seats = votes / sum(votes) * seats,
      eligible = votes > threshold_votes |
        (fixed_seats > 0 & fixed_seat_overrides_threshold)
    )

  # expand grid of party/quota needed for 1:n seats
  .seat_quotas <- tidyr::expand_grid(party = parties, quota_votes) %>%
    dplyr::right_join(party_seats, by = dplyr::join_by(party)) %>%
    dplyr::mutate(
      # add rank-order for potential seats, lowest quota_votes (used) first
      rank = rank(quota_votes),
      .by = party,

      # any potential seat with a rank <= fixed seats is already assigned
      seat_is_fixed = rank <= fixed_seats
    ) %>%

    # potential seats must be eligible (i.e., meet threshold) or fixed
    dplyr::filter(eligible | seat_is_fixed) %>%
    
    # sort potential seats by fixed status, then remaining votes quota, and last
    # by a random component to settle ties
    dplyr::arrange(
      dplyr::desc(seat_is_fixed),
      dplyr::desc(votes - quota_votes),
      stats::rnorm(dplyr::n())
    ) 
    
  # unless lr_extra_seat_resolution_method == FALSE, we can only give seats 
  # if the party has at least a claim to a partial seat.
  if (!isFALSE(lr_extra_seat_resolution_method)) {
    .seat_quotas <- .seat_quotas %>% dplyr::filter(seat_is_fixed | rank <= ceiling(votes / quota))
  }

  # to get allocated seats, we simply select the top n rows
  .seats <- .seat_quotas %>% 
    dplyr::slice_head(n = seats) %>%

    # summarize as seats per party
    dplyr::count(party, name = "seats") %>%

    # add back in party-level data we started with, and fill in seats = 0 for
    # parties that did not get any seats
    dplyr::right_join(party_seats, by = dplyr::join_by(party)) %>%
    tidyr::replace_na(list(seats = 0)) %>%

    # some final cleanup...
    dplyr::mutate(allocated_seats = seats - fixed_seats) %>%
    dplyr::relocate(
      party,
      votes,
      seats,
      fixed_seats,
      allocated_seats,
      ideal_seats
    )
  
  # if necessary, allocate remaining seats
  if (!isFALSE(lr_extra_seat_resolution_method) && sum(.seats$seats) < seats){
    # do another iteration of seat assignment, treating assigned seats as fixed
    message("There are more remainder seats than there are parties! Allocating extra remainder seats using ", lr_extra_seat_resolution_method)
    .extra_seats <- allocate_seats(
      parties = .seats$party,
      votes = .seats$votes,
      seats = seats,
      fixed_seats = .seats$seats,
      method = lr_extra_seat_resolution_method,
      threshold = threshold,
      fixed_seat_overrides_threshold = fixed_seat_overrides_threshold
    )

    dplyr::left_join(.seats, .extra_seats |> select(party, total_seats = seats), by = join_by(party)) |>
      dplyr::mutate(seats = total_seats, allocated_seats = seats - fixed_seats)
  } else {
    .seats
  }
}

.allocate_seats_la <- function(
  parties,
  votes,
  seats,
  fixed_seats,
  method,
  threshold,
  fixed_seat_overrides_threshold
) {
  # calculate quota depending on allocation method (only dHondt is implemented for now)
  if (method == "dh") {
    divisor = 1:seats
  } else {
    stop(glue::glue("allocation method not implemented: {method}"))
  }

  # votes required to meet threshold
  threshold_votes = sum(votes) * threshold

  # create party data
  party_seats <- tibble::tibble(party = parties, votes, fixed_seats) %>%
    dplyr::mutate(
      ideal_seats = votes / sum(votes) * seats,
      eligible = votes > threshold_votes |
        (fixed_seats > 0 & fixed_seat_overrides_threshold)
    )

  # create quotients list
  quotients <- tidyr::expand_grid(party = parties, divisor) %>%
    dplyr::left_join(party_seats, by = dplyr::join_by(party)) %>%
    dplyr::mutate(quotient = votes / divisor) %>%
    dplyr::mutate(quotient_rank = rank(-quotient), .by = party) %>%

    # mark seats that were already allocated (in districts) as assigned, so we
    # can include them regardless of the quotient.
    dplyr::mutate(seat_is_fixed = quotient_rank <= fixed_seats)

  # order by already assigned, max quotient, and break ties with a random component
  # then take the top rows as seat winners.
  #
  # Note that ordering by assigned first makes sure that previously assigned seats
  # are included, even if they would not be allocated based on the quotient alone.
  # Including a random component solves the tie-breaking problem without having
  # to implement an additional check.
  quotients %>%
    dplyr::filter(eligible | seat_is_fixed) %>%
    dplyr::arrange(
      dplyr::desc(seat_is_fixed),
      dplyr::desc(quotient),
      stats::rnorm(dplyr::n())
    ) %>%
    dplyr::slice_head(n = seats) %>%
    dplyr::count(party, name = "seats") %>%
    dplyr::right_join(party_seats, by = dplyr::join_by(party)) %>%
    tidyr::replace_na(replace = list(seats = 0)) %>%
    dplyr::mutate(allocated_seats = seats - fixed_seats) %>%
    dplyr::relocate(
      party,
      votes,
      seats,
      fixed_seats,
      allocated_seats,
      ideal_seats
    )
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
  allocate_seats(parties, votes, 150, threshold = 1 / 150, method = "dh")
}

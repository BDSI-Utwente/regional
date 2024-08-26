
#' Simulate vote shares for a given list of parties and districts.
#'
#' @param parties vector of party names, or a single integer for the number of parties
#' @param districts vector of district names, or a single integer for the number of districts
#' @param func function used to generate vote shares in each district. Is called as `func(N)`
#'      for each district, where `N` is the number of parties.
#'      Defaults to `simulate_shares_dichlet`.
#'
#' @return tibble containing three columns:
#'      \item{party} the party name (or index)
#'      \item{district} the district name (or index)
#'      \item{vote_share} vote share for this party, in this district.
#'
#' @details Vote shares are calculated within each district, and sum to 1 for each
#'      district. The parties participating in each district are assumed to be the
#'      same for each district.
#'
#' @export
#' @importFrom purrr list_rbind map
#' @importFrom tibble tibble
#'
#' @examples
#'
#' # Simulate vote shares for 16 parties in 12 districts, using the default
#' # dirichlet distribution.
#' simulate_shares(16, 12)
#'
#' # specify a known list of parties and districts
#' parties <- c("Green", "Red", "Blue", "Polka dots")
#' districts <- paste("District", 1:12)
#' simulate_shares(parties, districts)
#'
#' # specify a different vote share distribution function, and apply a normalization
#' # step to ensure vote shares within each district sum up to 1.
#' simulate_shares(parties, districts, func = runif, normalize = TRUE)
#'
#' # the distribution function maintains party order, one may want to give parties
#' # a vote share depending on their order to maintain some consistency across districts.
#' # E.g., for a fixed linear party size from largest to smallest party:
#' vote_share_linear <- function(n_parties) {
#'    n_parties:1
#' }
#' simulate_shares(parties, districts, func = vote_share_linear, normalize = TRUE)
#'
simulate_shares <- function(parties, districts, func = simulate_shares_dirichlet, normalize = FALSE) {
  if (length(districts) == 1 & is.numeric(districts)) {
    districts <- 1:districts
  }
  if (length(parties) == 1 & is.numeric(parties)) {
    parties <- 1:parties
  }

  purrr::map(districts, \(district) {
    tibble::tibble(
      district,
      party = parties,
      vote_share = func(length(parties)) %>%
        .normalize_if_needed(normalize)
    )
  }) %>%
    purrr::list_rbind()
}

.normalize_if_needed <- function(x, normalize) {
  if (normalize) {
    x / sum(x)
  } else {
    x
  }
}

#' Simulate vote shares based on a dirichlet distribution, using parameters
#' estimated by CITATION NEEDED
#'
#' @param n_parties integer, number of parties in each district.
#'
#' @return a vector of vote shares of length `n_parties` that sums to 1.
#' @export
#'
#' @examples
simulate_shares_dirichlet <- function(n_parties) {
  # sharesimulatoR::simulate_shares always returns 4k sims if taking posterior draws, we just need the one.
  # TODO: implement our own version using the posterior data from sharesimulatoR package.
  sharesimulatoR::simulate_shares(n_parties, "vote-winning parties", "Posterior draws of alpha", n_sim = 1)[1,]
}


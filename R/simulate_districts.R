#' Helper function to simulate district population size and turnout rates.
#'
#'
#' @param districts, vector of district names or integer number of districts.
#' @param func_population, function called to get district population sizes, defaults
#'  to an anonymous function that returns `15e6` (i.e., 15 million) equally distributed
#'  over the number of districts. This number is chosen to be roughly analogous to the
#'  total number of voters in the Netherlands. This function is called with a single
#'  argument `n_districts`, and should return a vector of length `n_districts`.
#' @param func_turnout, function called to get district turnout rates. Defaults to
#'  a uniform distribution with min = 0.5 and max = 0.7. This function is called with a single
#'  argument `n_districts`, and should return a vector of length `n_districts`. Turnout rates
#'  should be in the range 0-1.
#'
#' @return a tibble with three columns:
#'   \item{district}{district name (or index)}
#'   \item{district_population}{district population}
#'   \item{district_turnout}{district turnout rate}
#'
#' @export
#' @examples
#' # Simulate a vector of districts with some basic data
#' simulate_districts(paste("District", 1:12))
#'
#' # supply functions for voter population and turnout range
#' # random population in 300.000 - 1.000.000 range
#' fn_population <- \(n) runif(n, 0.3, 1) * 1e6
#' # fixed 80% turnout
#' fn_turnout <- \(n) 0.8
#' simulate_districts(12, fn_population, fn_turnout)
#'
simulate_districts <- function(
  districts,
  func_population = \(n_districts) rep(15e6 / n_districts, n_districts),
  func_turnout = \(n_districts) stats::runif(n_districts, 0.5, 0.7)
) {
  if (length(districts) == 1 & is.numeric(districts)) {
    districts <- 1:districts
  }
  n_districts <- length(districts)

  tibble::tibble(
    district = districts,
    population = func_population(n_districts),
    turnout = func_turnout(n_districts)
  )
}

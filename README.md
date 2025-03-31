
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regional

<!-- badges: start -->

<!-- badges: end -->

The `regional` package is tailored to simulate the effects of different
permutations of multi-seat district electoral systems on party and
regional proportionality. The package is heavily inspired by
`electoral`, and covers broadly the same functionality (with some
notable additions), but is rewritten from the ground up.

The package is split into three (sets of) functions, that each handle a
specific step in simulating elections and assessing their outcomes.
These functions can be combined in various ways to simulate a wide
variety of electoral systems and conditions.

`simulate_districts` and `simulate_shares` allow the user to easily
simulate districts and vote shares under realistic (or if desired,
unrealistic) circumstances. `allocate_seats` implements d’Hondt and Hare
seat allocation, while `saint_langue_index`, `gallagher_index`,
`loosemore_hanby_index`, and `seat_difference` implement a variety of
indeces of election (dis)- proportionality.

In comparison to package `electoral`, this package’s version of
`allocate_seats`:

- allows for ‘fixed’ seats, making a wide range of multi-stage electoral
  systems possible to simulate
- allows for vote share thresholds in seat allocation
- implements only d’Hondt and Hare allocation methods

## Installation

You can install the development version of regional from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("BDSI-Utwente/regional")
```

## Example

``` r
library(regional)
```

### Simulating districts

The `simulate_districts` function can generate districts with random
population sizes and turnout rates. Note that it is entirely possible to
bypass this function, and use a known set of districts instead.

``` r
# generate 12 districts, with equal sizes and uniform(0.5, 0.7) turnout rates
districts <- simulate_districts(12) 
districts
#> # A tibble: 12 × 3
#>    district population turnout
#>       <int>      <dbl>   <dbl>
#>  1        1    1250000   0.662
#>  2        2    1250000   0.612
#>  3        3    1250000   0.578
#>  4        4    1250000   0.547
#>  5        5    1250000   0.573
#>  6        6    1250000   0.550
#>  7        7    1250000   0.563
#>  8        8    1250000   0.548
#>  9        9    1250000   0.604
#> 10       10    1250000   0.659
#> 11       11    1250000   0.694
#> 12       12    1250000   0.552

# we can specify different functions for population size and turnout rates
districts <- simulate_districts(
  districts = paste("District", 1:13),
  func_population = \(x) rnorm(x, 1e6, 1e5),
  func_turnout = \(x) rep(0.7, x))
districts
#> # A tibble: 13 × 3
#>    district    population turnout
#>    <chr>            <dbl>   <dbl>
#>  1 District 1     767946.     0.7
#>  2 District 2    1029615.     0.7
#>  3 District 3     983082.     0.7
#>  4 District 4     939586.     0.7
#>  5 District 5    1092659.     0.7
#>  6 District 6     924216.     0.7
#>  7 District 7    1045806.     0.7
#>  8 District 8     952431.     0.7
#>  9 District 9     949910.     0.7
#> 10 District 10   1086541.     0.7
#> 11 District 11    911982.     0.7
#> 12 District 12    988038.     0.7
#> 13 District 13    915513.     0.7

# or skip this step, and manually specify a tibble...
districts_manual <- tibble::tibble(
  district = c("North", "East", "South", "West"),
  population = c(1.2e6, 1.4e6, 0.8e6, 3.2e6),
  turnout = c(0.65, 0.71, 0.64, 0.68)
)
districts_manual
#> # A tibble: 4 × 3
#>   district population turnout
#>   <chr>         <dbl>   <dbl>
#> 1 North       1200000    0.65
#> 2 East        1400000    0.71
#> 3 South        800000    0.64
#> 4 West        3200000    0.68
```

### Simulating vote shares

The `simulate_shares` function is a wrapper to generate vote shares for
a given list of parties, for each of a given list of districts. By
default, it uses a Dirichlet distribution with parameters fitted to
historical election results by <CITATION NEEDED>, but this can be
overridden.

As with districts, this function should take the number of parties as
it’s only argument. The default implementation assumes parties are
ordered from large to small, and generates semi-consistent results
across each district. Note that vote shares should sum to 1 in each
district, the `simulate_shares` function has an optional `normalize`
argument that will enforce this requirement for you.

``` r
# generate vote shares for districts generated above
vote_shares <- simulate_shares(12, districts$district)
vote_shares
#> # A tibble: 156 × 3
#>    district   party vote_share
#>    <chr>      <int>      <dbl>
#>  1 District 1     1   0.249   
#>  2 District 1     2   0.145   
#>  3 District 1     3   0.105   
#>  4 District 1     4   0.132   
#>  5 District 1     5   0.160   
#>  6 District 1     6   0.136   
#>  7 District 1     7   0.0182  
#>  8 District 1     8   0.0226  
#>  9 District 1     9   0.0292  
#> 10 District 1    10   0.000641
#> # ℹ 146 more rows

# we can specify party names, and override the vote share function.
# using a uniform distribution and normalizing manually
simulate_shares(c("Green", "Red", "Blue", "Polka dots"), districts$district, func = \(x) {
  shares <- runif(x)
  shares / sum(shares)
})
#> # A tibble: 52 × 3
#>    district   party      vote_share
#>    <chr>      <chr>           <dbl>
#>  1 District 1 Green          0.199 
#>  2 District 1 Red            0.205 
#>  3 District 1 Blue           0.322 
#>  4 District 1 Polka dots     0.274 
#>  5 District 2 Green          0.272 
#>  6 District 2 Red            0.287 
#>  7 District 2 Blue           0.427 
#>  8 District 2 Polka dots     0.0148
#>  9 District 3 Green          0.144 
#> 10 District 3 Red            0.0797
#> # ℹ 42 more rows

# a deterministic linear vote share, letting `simulate_shares` normalize for us
simulate_shares(16, 12, \(x) x:1, normalize = TRUE)
#> # A tibble: 192 × 3
#>    district party vote_share
#>       <int> <int>      <dbl>
#>  1        1     1     0.118 
#>  2        1     2     0.110 
#>  3        1     3     0.103 
#>  4        1     4     0.0956
#>  5        1     5     0.0882
#>  6        1     6     0.0809
#>  7        1     7     0.0735
#>  8        1     8     0.0662
#>  9        1     9     0.0588
#> 10        1    10     0.0515
#> # ℹ 182 more rows
```

While we could use vote shares to allocate seats, we’ll want to
calculate vote counts so that any ‘equalization seats’ allocated
nationally are allocated proportional to voter counts in the districts.

``` r
votes <- vote_shares |> 
  dplyr::left_join(districts) |> 
  dplyr::mutate(vote_count = vote_share * turnout * population) |> 
  dplyr::select(district, party, dplyr::starts_with("vote_"))
#> Joining with `by = join_by(district)`
```

The result is again a simple tibble with vote shares and counts per
district and party, we could easily replace this tibble with historical
results. We’ll leave that as an exercise for the reader.

### Allocating seats

Knowing vote counts, we can allocate seats using some common seat
allocation recipes with the `allocate_seats` function. Currently, only
d’Hondt (`dh`), and Hare (`hare`) are implemented.

The implementation of these recipes is derived from the `electoral`
package (CITATION NEEDED), with added support for vote thresholds and
accounting for ‘fixed’ seats allocated previously. The main intended use
case is to allocate a number of seats within the regions, before
allocating a number of ‘equalization seats’ to ensure proportionality of
national election results.

> TODO: Thresholds are not yet implemented

The above implies three rounds of seat allocation: (1) the number of
seats per district, (2) the number of seats per party within each
district, and (3) the number of seats per party, compensating for seats
already allocated in the districts. We’ll run through an example where
for a 150 seat parliament, 100 of which are allocated within the
districts, and the remaining 50 as equalization seats.

``` r
# allocating seats to districts
allocate_seats(districts$district, districts$population, 100)
#> # A tibble: 13 × 7
#>    party          votes seats fixed_seats allocated_seats ideal_seats eligible
#>    <chr>          <dbl> <int>       <dbl>           <dbl>       <dbl> <lgl>   
#>  1 District 5  1092659.     9           0               9        8.68 TRUE    
#>  2 District 10 1086541.     9           0               9        8.63 TRUE    
#>  3 District 7  1045806.     8           0               8        8.31 TRUE    
#>  4 District 2  1029615.     8           0               8        8.18 TRUE    
#>  5 District 12  988038.     8           0               8        7.85 TRUE    
#>  6 District 3   983082.     8           0               8        7.81 TRUE    
#>  7 District 8   952431.     8           0               8        7.57 TRUE    
#>  8 District 9   949910.     8           0               8        7.55 TRUE    
#>  9 District 4   939586.     7           0               7        7.46 TRUE    
#> 10 District 6   924216.     7           0               7        7.34 TRUE    
#> 11 District 13  915513.     7           0               7        7.27 TRUE    
#> 12 District 11  911982.     7           0               7        7.25 TRUE    
#> 13 District 1   767946.     6           0               6        6.10 TRUE
districts$seats <- allocate_seats(districts$district, districts$population, 100)$seats
```

Allocating seats within districts requires some more data wrangling.

``` r
# within each district, allocate seats to parties
district_seats <- votes |> 
  
  # create a nested data frame with vote counts per district
  dplyr::nest_by(district) |> 
  
  # join in district metadata to get seats per district
  dplyr::left_join(districts) |> 
  
  # map allocation function to each row (district)
  purrr::pmap(\(district, seats, data, ...) {
    allocate_seats(data$party, data$vote_count, seats) |> 
      dplyr::mutate(district)
  }) |> 
  
  # combine the list of per-district results back into a single tibble
  purrr::list_rbind()
#> Joining with `by = join_by(district)`

# long format of per party, per district votes and seats
district_seats
#> # A tibble: 156 × 8
#>    party   votes seats fixed_seats allocated_seats ideal_seats eligible district
#>    <int>   <dbl> <int>       <dbl>           <dbl>       <dbl> <lgl>    <chr>   
#>  1     1 133756.     3           0               3      2.24   TRUE     Distric…
#>  2     5  86239.     2           0               2      1.44   TRUE     Distric…
#>  3     2  78194.     1           0               1      1.31   TRUE     Distric…
#>  4     6  73357.     1           0               1      1.23   TRUE     Distric…
#>  5     4  70788.     1           0               1      1.19   TRUE     Distric…
#>  6     3  56544.     1           0               1      0.947  TRUE     Distric…
#>  7     9  15685.     0           0               0      0.263  TRUE     Distric…
#>  8     8  12152.     0           0               0      0.203  TRUE     Distric…
#>  9     7   9807.     0           0               0      0.164  TRUE     Distric…
#> 10    11    648.     0           0               0      0.0108 TRUE     Distric…
#> # ℹ 146 more rows

# we don't really need the various extra columns, so let's clean up a bit
(district_seats <- district_seats |> 
  dplyr::transmute(district, party, votes, seats))
#> # A tibble: 156 × 4
#>    district   party   votes seats
#>    <chr>      <int>   <dbl> <int>
#>  1 District 1     1 133756.     3
#>  2 District 1     5  86239.     2
#>  3 District 1     2  78194.     1
#>  4 District 1     6  73357.     1
#>  5 District 1     4  70788.     1
#>  6 District 1     3  56544.     1
#>  7 District 1     9  15685.     0
#>  8 District 1     8  12152.     0
#>  9 District 1     7   9807.     0
#> 10 District 1    11    648.     0
#> # ℹ 146 more rows
```

Finally, we’ll want to aggregate the number of votes given in the
districts, and allocate remaining seats to ensure national party
proportionality.

``` r
# note that small parties have not been assigned any seats in any of the regions,
# as they likely never met the threshold for a seat in any single region. 
district_seats_summed <- district_seats |> 
  dplyr::summarize(votes = sum(votes), district_seats = sum(seats), .by = party)
district_seats_summed
#> # A tibble: 12 × 3
#>    party    votes district_seats
#>    <int>    <dbl>          <int>
#>  1     1 2192377.             34
#>  2     5 1413541.             15
#>  3     2 1281662.             13
#>  4     6 1202381.             13
#>  5     4 1160278.             13
#>  6     3  926809.             12
#>  7     9  257083.              0
#>  8     8  199189.              0
#>  9     7  160744.              0
#> 10    11   10621.              0
#> 11    10    5649.              0
#> 12    12     793.              0

# now allocate the full 150 seats, but 'fix' the seats already allocated in the districts
national_seats <- allocate_seats(district_seats_summed$party, 
                                 district_seats_summed$votes,
                                 150,
                                 district_seats_summed$district_seats)

national_seats
#> # A tibble: 12 × 7
#>    party    votes seats fixed_seats allocated_seats ideal_seats eligible
#>    <int>    <dbl> <int>       <int>           <int>       <dbl> <lgl>   
#>  1     1 2192377.    38          34               4     37.3    TRUE    
#>  2     5 1413541.    24          15               9     24.1    TRUE    
#>  3     2 1281662.    22          13               9     21.8    TRUE    
#>  4     6 1202381.    21          13               8     20.5    TRUE    
#>  5     4 1160278.    20          13               7     19.8    TRUE    
#>  6     3  926809.    16          12               4     15.8    TRUE    
#>  7     9  257083.     4           0               4      4.38   TRUE    
#>  8     8  199189.     3           0               3      3.39   TRUE    
#>  9     7  160744.     2           0               2      2.74   TRUE    
#> 10    11   10621.     0           0               0      0.181  TRUE    
#> 11    10    5649.     0           0               0      0.0962 TRUE    
#> 12    12     793.     0           0               0      0.0135 TRUE
```

Note that these national seats are not affiliated with a specific
district, one may want to implement yet another round of allocation to
distribute these seats to candidates from the districts, but we’ll again
leave that as an exercise for the reader.

### Proportionality of results

The goal of this analysis is to simulate results using different
conditions, and compare the proportionality of results. The
`get_disproportionality_indices` function implements (will implement)
several common measures of proportionality, and can be used to check the
proportionality of results across parties, districts, or any other
attribute.

``` r
# (dis) proportionality across parties
get_disproportionality_indices(national_seats$votes, national_seats$seats)
#>       sli        gi       lhi      diff 
#> 0.4000876 0.6266092 0.0123903 0.0000000
```

Calculating regional proportionality is slightly more involved, and
further complicated by the addition of national equalization seats not
directly associated with any region. For simplicity, let’s assume all
these seats are allocated to candidates associated with the capital
district of our example nation, District 1.

``` r
# we have the baseline voter counts and number of seats per district already
per_district_seats <- districts |>
  dplyr::transmute(district, seats, votes = population * turnout)

# we'll add the equalization seats to District 1 (which we've checked is the first row)
per_district_seats[1, "seats"] <- per_district_seats[1, "seats"] + 50
```

We can then simply substitute parties for districts and votes for
population.

``` r
get_disproportionality_indices(per_district_seats$votes, per_district_seats$seats)
#>         sli          gi         lhi        diff 
#> 193.1614588  24.4990847   0.3323239  50.0000000
```

Disproportionality across regions is higher, presumably because we’ve
artificially (but not entirely unrealistically) assigned all remainder
seats to a single region.

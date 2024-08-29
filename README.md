
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regional

<!-- badges: start -->
<!-- badges: end -->

The `regional` package is tailored to simulate the effects of different
permutations of multi-seat district electoral systems on party and
regional proportionality.

The package is split into four main functions, that each handle a
specific step in simulating elections. They can be combined in various
ways to simulate a wide variety of electoral systems and conditions.

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
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
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
#>  1        1    1250000   0.504
#>  2        2    1250000   0.610
#>  3        3    1250000   0.689
#>  4        4    1250000   0.550
#>  5        5    1250000   0.631
#>  6        6    1250000   0.536
#>  7        7    1250000   0.616
#>  8        8    1250000   0.662
#>  9        9    1250000   0.691
#> 10       10    1250000   0.617
#> 11       11    1250000   0.672
#> 12       12    1250000   0.644

# we can specify different functions for population size and turnout rates
districts <- simulate_districts(
  districts = paste("District", 1:13),
  func_population = \(x) rnorm(x, 1e6, 1e5),
  func_turnout = \(x) rep(0.7, x))
districts
#> # A tibble: 13 × 3
#>    district    population turnout
#>    <chr>            <dbl>   <dbl>
#>  1 District 1     997405.     0.7
#>  2 District 2     977601.     0.7
#>  3 District 3    1036709.     0.7
#>  4 District 4    1021277.     0.7
#>  5 District 5    1225111.     0.7
#>  6 District 6    1025316.     0.7
#>  7 District 7    1002956.     0.7
#>  8 District 8     877659.     0.7
#>  9 District 9     860684.     0.7
#> 10 District 10    884012.     0.7
#> 11 District 11   1031741.     0.7
#> 12 District 12    840470.     0.7
#> 13 District 13   1075569.     0.7

# or skip this step, and manually specify a tibble...
districts_manual <- tibble(
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
votes <- vote_shares %>% 
  left_join(districts) %>% 
  mutate(vote_count = vote_share * turnout * population) %>% 
  select(district, party, starts_with("vote_"))
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
#>  1 District 1   997405.     8           0               8        7.76 TRUE    
#>  2 District 10  884012.     7           0               7        6.88 TRUE    
#>  3 District 11 1031741.     8           0               8        8.03 TRUE    
#>  4 District 12  840470.     6           0               6        6.54 TRUE    
#>  5 District 13 1075569.     8           0               8        8.37 TRUE    
#>  6 District 2   977601.     7           0               7        7.60 TRUE    
#>  7 District 3  1036709.     8           0               8        8.06 TRUE    
#>  8 District 4  1021277.     8           0               8        7.94 TRUE    
#>  9 District 5  1225111.    10           0              10        9.53 TRUE    
#> 10 District 6  1025316.     8           0               8        7.98 TRUE    
#> 11 District 7  1002956.     8           0               8        7.80 TRUE    
#> 12 District 8   877659.     7           0               7        6.83 TRUE    
#> 13 District 9   860684.     7           0               7        6.69 TRUE
districts$seats <- allocate_seats(districts$district, districts$population, 100)$seats
```

Allocating seats within districts requires some more data wrangling.

``` r
# within each district, allocate seats to parties
district_seats <- votes %>% 
  
  # create a nested data frame with vote counts per district
  nest_by(district) %>% 
  
  # join in district metadata to get seats per district
  left_join(districts) %>% 
  
  # map allocation function to each row (district)
  purrr::pmap(\(district, seats, data, ...) {
    allocate_seats(data$party, data$vote_count, seats) %>% 
      mutate(district)
  }) %>% 
  
  # combine the list of per-district results back into a single tibble
  purrr::list_rbind()
#> Joining with `by = join_by(district)`

# long format of per party, per district votes and seats
district_seats
#> # A tibble: 156 × 8
#>    party   votes seats fixed_seats allocated_seats ideal_seats eligible district
#>    <int>   <dbl> <int>       <dbl>           <dbl>       <dbl> <lgl>    <chr>   
#>  1     1 173721.     3           0               3     1.99    TRUE     Distric…
#>  2     2 101557.     1           0               1     1.16    TRUE     Distric…
#>  3     3  73439.     1           0               1     0.841   TRUE     Distric…
#>  4     4  91939.     1           0               1     1.05    TRUE     Distric…
#>  5     5 112007.     1           0               1     1.28    TRUE     Distric…
#>  6     6  95275.     1           0               1     1.09    TRUE     Distric…
#>  7     7  12737.     0           0               0     0.146   TRUE     Distric…
#>  8     8  15784.     0           0               0     0.181   TRUE     Distric…
#>  9     9  20371.     0           0               0     0.233   TRUE     Distric…
#> 10    10    448.     0           0               0     0.00513 TRUE     Distric…
#> # ℹ 146 more rows

# we don't really need the various extra columns, so let's clean up a bit
(district_seats <- district_seats %>% 
  transmute(party, district, votes, seats))
#> # A tibble: 156 × 4
#>    party district     votes seats
#>    <int> <chr>        <dbl> <int>
#>  1     1 District 1 173721.     3
#>  2     2 District 1 101557.     1
#>  3     3 District 1  73439.     1
#>  4     4 District 1  91939.     1
#>  5     5 District 1 112007.     1
#>  6     6 District 1  95275.     1
#>  7     7 District 1  12737.     0
#>  8     8 District 1  15784.     0
#>  9     9 District 1  20371.     0
#> 10    10 District 1    448.     0
#> # ℹ 146 more rows
```

Finally, we’ll want to calculate the number of votes given in the
districts, and allocate remaining seats to ensure national party
proportionality.

``` r
district_seats_summed <- district_seats %>% 
  summarize(votes = sum(votes), district_seats = sum(seats), .by = party)
district_seats_summed
#> # A tibble: 12 × 3
#>    party    votes district_seats
#>    <int>    <dbl>          <int>
#>  1     1 2239262.             34
#>  2     2 1309071.             14
#>  3     3  946629.             12
#>  4     4 1185091.             13
#>  5     5 1443770.             14
#>  6     6 1228095.             13
#>  7     7  164181.              0
#>  8     8  203449.              0
#>  9     9  262581.              0
#> 10    10    5770.              0
#> 11    11   10848.              0
#> 12    12     810.              0

# now allocate the full 150 seats, but 'fix' the seats already allocated in the districts
national_seats <- allocate_seats(district_seats_summed$party, 
                                 district_seats_summed$votes,
                                 150,
                                 district_seats_summed$district_seats)

national_seats
#> # A tibble: 12 × 7
#>    party    votes seats fixed_seats allocated_seats ideal_seats eligible
#>    <int>    <dbl> <int>       <int>           <int>       <dbl> <lgl>   
#>  1     1 2239262.    38          34               4     37.3    TRUE    
#>  2     2 1309071.    22          14               8     21.8    TRUE    
#>  3     3  946629.    16          12               4     15.8    TRUE    
#>  4     4 1185091.    20          13               7     19.8    TRUE    
#>  5     5 1443770.    24          14              10     24.1    TRUE    
#>  6     6 1228095.    21          13               8     20.5    TRUE    
#>  7     7  164181.     2           0               2      2.74   TRUE    
#>  8     8  203449.     3           0               3      3.39   TRUE    
#>  9     9  262581.     4           0               4      4.38   TRUE    
#> 10    10    5770.     0           0               0      0.0962 TRUE    
#> 11    11   10848.     0           0               0      0.181  TRUE    
#> 12    12     810.     0           0               0      0.0135 TRUE
```

Note that these national seats are not affiliated with a specific
district, one may want to implement yet another round of allocation to
distribute these seats to candidates from the districts, but we’ll again
leave that as an exercise for the reader.

### Proportionality of results

The goal of this analysis is to simulate results using different
conditions, and compare the proportionality of results. The
`check_proportionality` function implements (will implement) several
common measures of proportionality, and can be used to check the
proportionality of results across parties, districts, or any other
attribute.

``` r
# (dis) proportionality across parties
get_disproportionality_indices(national_seats$votes, national_seats$seats)
#>       sli        gi       lhi 
#> 0.4000876 0.6266092 0.0123903
```

Calculating regional proportionality is slightly more involved, and
further complicated by the addition of national equalization seats not
directly associated with any region. For simplicity, let’s assume all
these seats are allocated to candidates associated with the capital
district of our example nation, District 1.

``` r
# we have the baseline voter counts and number of seats per district already
per_district_seats <- districts %>%
  transmute(district, seats, votes = population * turnout)

# we'll add the equalization seats to District 1 (which we've checked is the first row)
per_district_seats[1, "seats"] <- per_district_seats[1, "seats"] + 50
```

We can then simply substitute parties for districts, votes for
population, and so forth.

``` r
get_disproportionality_indices(per_district_seats$votes, per_district_seats$seats)
#>         sli          gi         lhi 
#> 134.9081210  22.9200978   0.3090869
```

Disproportionality across regions is higher, presumably because we’ve
artificially (but not entirely unrealistically) assigned all remainder
seats to a single region.

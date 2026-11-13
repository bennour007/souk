# two players, two goods, each better at a different one
two_traders <- function() {
  v <- new_village(n_players = 2, n_goods = 2)
  v$players$eff[]     <- c(1, 0.5, 0.5, 1)   # player 1: (1, 0.5); player 2: (0.5, 1)
  v$players$eff_start <- v$players$eff
  v$players$choice    <- c(1L, 2L)
  v
}

# play the day up to and including the market
to_market <- function(v, rules) {
  v |> step_events(rules) |> step_market(rules)
}

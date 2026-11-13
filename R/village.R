#' Create a village
#'
#' Draws each player's starting efficiency in each good and returns the
#' village on day 0. Every player starts by specialising in the good they are
#' most efficient at, outside any group.
#'
#' A village is a list with four parts:
#' \describe{
#'   \item{`players`}{`eff`: a players-by-goods matrix of units per hour.
#'     `eff_start`: the same on day 0. `new_eff`: efficiency at the new
#'     product, `NA` until known. `exposure`: purchases of the new product.
#'     `choice`: the good each player makes, as a column number, or 0 for
#'     none. `group`: the player's group id, or `NA`. `free`: yesterday's
#'     free hours.}
#'   \item{`groups`}{A tibble with one row per group ever founded: `id`,
#'     `founded` (day) and `tax` (whether it taxes outsiders). A group's id
#'     is its row number, and ids are never reused.}
#'   \item{`world`}{The day, the seed, and the state of droughts and the
#'     invention.}
#'   \item{`today`}{Values that only describe the current day, such as hours
#'     worked and swaps made. It is emptied every morning.}
#' }
#'
#' @param n_players Number of players.
#' @param n_goods Number of goods, from 2 to 26.
#' @param mean_efficiency Typical starting efficiency, in units per hour.
#' @param sigma Spread of starting efficiencies, on the log scale.
#' @param seed A whole number from 1 to 2000. It fixes the starting draw and
#'   every random draw in later days, so the same village and rules always
#'   give the same run.
#'
#' @return A village, of class `souk_village`.
#' @export
#'
#' @examples
#' village <- new_village(n_players = 6, n_goods = 3, seed = 1)
#' village
#' village$players$eff
new_village <- function(n_players = 100, n_goods = 8, mean_efficiency = 0.8,
                        sigma = 0.3, seed = 1) {
  stopifnot(
    "`n_players` must be at least 1"          = n_players >= 1,
    "`n_goods` must be between 2 and 26"      = n_goods >= 2 && n_goods <= 26,
    "`seed` must be a whole number, 1 to 2000" =
      seed == round(seed) && seed >= 1 && seed <= 2000
  )

  withr::with_seed(seed, {
    eff <- matrix(
      mean_efficiency * exp(rnorm(n_players * n_goods, mean = 0, sd = sigma)),
      nrow = n_players, dimnames = list(NULL, LETTERS[seq_len(n_goods)])
    )
    drought_start <- sample.int(8, 1)
  })

  structure(
    list(
      players = list(
        eff       = eff,
        eff_start = eff,
        new_eff   = matrix(NA_real_, n_players, n_goods),
        exposure  = numeric(n_players),
        choice    = max.col(eff, ties.method = "first"),
        group     = rep(NA_integer_, n_players),
        free      = rep(NA_real_, n_players)
      ),
      groups = tibble(id = integer(), founded = integer(), tax = logical()),
      world  = list(
        day = 0L, seed = as.integer(seed),
        drought_good = NA_integer_, drought_start = drought_start,
        drought_left = 0L, droughts_done = 0L,
        new_need = NA_integer_, inventor = NA_integer_
      ),
      today = list()
    ),
    class = "souk_village"
  )
}

#' @export
print.souk_village <- function(x, ...) {
  cat(sprintf(
    "A village of %d players and %d goods, on day %d, with %d groups founded.\n",
    nrow(x$players$eff), ncol(x$players$eff), x$world$day, nrow(x$groups)
  ))
  invisible(x)
}

#' Count the groups with enough members
#'
#' @param group Each player's group id, with `NA` for players outside a group.
#' @param min_members Fewest members for a group to count.
#'
#' @return A single number.
#' @export
#'
#' @examples
#' count_groups(c(1, 1, 2, NA))
count_groups <- function(group, min_members = 2) {
  sum(tabulate(group[!is.na(group)]) >= min_members)
}

#' Check that a day's books balance
#'
#' Stops with a named message if the day just played breaks one of the
#' model's accounting rules: every swap has a counterpart, taxes paid equal
#' taxes received, efficiencies are positive and finite, free hours lie
#' within the day, and every player's group exists.
#'
#' [play_day()] calls this after every day when `rules$world$check` is
#' `TRUE`. Call it yourself after a custom step, while the day's values are
#' still in `v$today`.
#'
#' @param v A village that has been through [step_market()] today.
#' @param rules Rules from [set_rules()].
#'
#' @return The village, invisibly.
#' @export
check_village <- function(v, rules) {
  p <- v$players
  t <- v$today
  G <- ncol(p$eff)

  made   <- p$choice[row(t$matched)[t$matched]]
  wanted <- col(t$matched)[t$matched]
  flows  <- matrix(tabulate(made + G * (wanted - 1L), G * G), G)

  stopifnot(
    "swaps don't balance"     = all(flows == t(flows)),
    "taxes don't balance"     = abs(sum(t$tax_paid) - sum(t$tax_received)) < 1e-8,
    "efficiency out of range" = all(is.finite(p$eff)) && all(p$eff > 0),
    "free hours out of range" = all(p$free >= 0 & p$free <= rules$world$day_length),
    "unknown group"           = all(is.na(p$group) | p$group <= nrow(v$groups))
  )
  invisible(v)
}

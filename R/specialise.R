#' The chance of finding a swap
#'
#' Given what everyone makes, returns the chance that a maker of good `k`
#' finds a partner to swap for good `j`. If there are at least as many makers
#' of `j` as of `k`, the chance is 1. Otherwise it is their ratio, because
#' swaps are one for one.
#'
#' @param choice The good each player makes, as a column number; 0 for none.
#' @param n_goods Number of goods.
#'
#' @return A goods-by-goods matrix: row `k`, column `j`. The diagonal is 0.
#' @export
#'
#' @examples
#' # two makers of good 1, one maker of good 2
#' swap_chances(c(1, 1, 2), n_goods = 2)
swap_chances <- function(choice, n_goods) {
  makers <- tabulate(choice[choice > 0], n_goods)
  chance <- pmin(outer(1 / pmax(makers, 1), makers), 1)
  diag(chance) <- 0
  chance
}

#' The hours each option would cost a player
#'
#' Specialising in good `k` costs the hours for the player's own unit and for
#' one unit per expected swap, the time the swaps take, and the hours for
#' whatever they expect to make themselves because no swap is found. The last
#' column is the cost of making everything alone.
#'
#' @param unit_hours A players-by-goods matrix of hours per unit.
#' @param chance A matrix from [swap_chances()].
#' @param swap_cost Hours one swap takes.
#'
#' @return A matrix with one row per player and one column per option: each
#'   good, then `alone`.
#' @export
#'
#' @examples
#' unit_hours <- matrix(c(2, 4), nrow = 1, dimnames = list(NULL, c("A", "B")))
#' option_costs(unit_hours, swap_chances(c(1, 2), 2))
option_costs <- function(unit_hours, chance, swap_cost = 0) {
  swaps <- rowSums(chance)                                  # expected swaps, by option
  cost  <- unit_hours %*% t(1 - chance) +
    sweep(unit_hours, 2, swaps, `*`) +
    rep(swap_cost * swaps, each = nrow(unit_hours))
  colnames(cost) <- colnames(unit_hours)
  cbind(cost, alone = rowSums(unit_hours))
}

#' Step: players choose what to make
#'
#' Each player reconsiders with chance `rules$player$revise`, and then takes
#' the option that costs the fewest hours given what everyone makes now; see
#' [option_costs()]. The others keep their specialty.
#'
#' Players judge by the whole village and ignore the priority their group
#' gives them. To try another decision rule, write a step with the same
#' arguments and put it in the schedule in place of this one.
#'
#' @inheritParams event_drought
#'
#' @return The village, with `players$choice` updated.
#' @export
step_specialise <- function(v, rules) {
  p     <- v$players
  G     <- ncol(p$eff)
  costs <- option_costs(v$today$unit_hours, swap_chances(p$choice, G),
                        rules$market$swap_cost)

  best <- max.col(-costs, ties.method = "first")
  best[best == G + 1L] <- 0L                                # making everything alone

  revises <- runif(length(best)) < rules$player$revise
  v$players$choice <- if_else(revises, best, p$choice)
  v
}

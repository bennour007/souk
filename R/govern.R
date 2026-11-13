#' Turn members' votes into a group decision
#'
#' @param yes Logical, one per voting member.
#' @param weight Positive numbers, one per voting member. Only used by rules
#'   that weight votes.
#' @param voting How votes are counted. `"majority"`: more than half of the
#'   members. `"weighted"`: more than half of the total weight. Or a function
#'   of `yes` and `weight` that returns `TRUE` or `FALSE`, for a rule of your
#'   own.
#'
#' @return `TRUE` if the proposal passes.
#' @export
#'
#' @examples
#' yes <- c(TRUE, TRUE, FALSE)
#' tally_votes(yes, voting = "majority")
#' tally_votes(yes, weight = c(1, 1, 10), voting = "weighted")
#'
#' # unanimity, as a custom rule
#' tally_votes(yes, voting = function(yes, weight) all(yes))
tally_votes <- function(yes, weight = rep(1, length(yes)), voting = "majority") {
  if (is.function(voting)) return(isTRUE(voting(yes, weight)))

  switch(
    voting,
    majority = mean(yes) > 0.5,
    weighted = sum(yes * weight) > 0.5 * sum(weight),
    stop("Unknown voting rule: ", voting, call. = FALSE)
  )
}

#' Step: groups decide on a tax
#'
#' Every `rules$tax$vote_every` days, each group votes on taxing outsiders.
#' A member is in favour if the tax would bring in more hours than it costs
#' them:
#'
#' `rate * outside swaps * s  >  outside swaps * (1 - s) * (hours for another good - hours for their own)`
#'
#' where `s` is the chance that a maker of their good found an open-market
#' swap today. With chance `disturbance`, a member votes the other way. Votes
#' are counted by [tally_votes()], weighted by swaps made, and the result is
#' stored in `v$groups$tax`.
#'
#' Does nothing when taxes are off or on non-voting days.
#'
#' @inheritParams event_drought
#'
#' @return The village, with `groups$tax` updated.
#' @export
step_govern <- function(v, rules) {
  x <- rules$tax
  if (!x$on || (v$world$day + 1L) %% x$vote_every != 0) return(v)

  p <- v$players
  m <- which(!is.na(p$group) & p$choice > 0)
  if (length(m) == 0) return(v)

  U     <- v$today$unit_hours
  own   <- U[cbind(m, p$choice[m])]
  other <- (rowSums(U)[m] - own) / (ncol(U) - 1)
  keep  <- v$today$sell[p$choice[m]]
  keep[is.nan(keep)] <- 1

  gain <- x$rate * v$today$outside[m] * keep
  loss <- v$today$outside[m] * (1 - keep) * (other - own)
  yes  <- xor(gain > loss, runif(length(m)) < x$disturbance)

  for (g in unique(p$group[m])) {
    voters <- p$group[m] == g
    v$groups$tax[g] <- tally_votes(yes[voters], v$today$swaps[m][voters] + 1, x$voting)
  }
  v
}

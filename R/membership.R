#' A player's decision: leave a group
#'
#' A member who had fewer free hours yesterday than the average player
#' outside any group leaves, with chance `rules$player$revise`.
#'
#' @inheritParams event_drought
#'
#' @return The village, with `players$group` updated.
#' @export
members_leave <- function(v, rules) {
  p      <- v$players
  bar    <- mean(p$free[is.na(p$group)], na.rm = TRUE)
  leaves <- which(!is.na(p$group) & p$free < bar &
                    runif(length(p$group)) < rules$player$revise)
  v$players$group[leaves] <- NA
  v
}

#' A group's decision: expel surplus makers
#'
#' A group keeps at most `rules$groups$max_same` makers of each good. When it
#' has more, because members changed specialty, those with the most free
#' hours yesterday stay and the rest are expelled.
#'
#' @inheritParams event_drought
#'
#' @return The village, with `players$group` updated.
#' @export
groups_expel <- function(v, rules) {
  p <- v$players
  m <- which(!is.na(p$group) & p$choice > 0)
  if (length(m) == 0) return(v)

  key   <- p$group[m] * (ncol(p$eff) + 1L) + p$choice[m]
  place <- ave(-coalesce(p$free[m], 0) + runif(length(m), 0, 1e-6), key, FUN = rank)
  v$players$group[m[place > rules$groups$max_same]] <- NA
  v
}

#' A player's decision: found a group
#'
#' On founding days (every `found_every` days, from day `found_from`), each
#' player outside a group founds one with chance `rules$groups$found`. A new
#' group gets the next id and a row in `v$groups`, and starts without a tax.
#'
#' @inheritParams event_drought
#'
#' @return The village, with `players$group` and `groups` updated.
#' @export
players_found <- function(v, rules) {
  g     <- rules$groups
  today <- v$world$day + 1L
  if (today < g$found_from || today %% g$found_every != 0) return(v)

  p        <- v$players
  founders <- which(is.na(p$group) & p$choice > 0 & runif(length(p$group)) < g$found)
  if (length(founders) == 0) return(v)

  ids <- nrow(v$groups) + seq_along(founders)
  v$players$group[founders] <- ids
  v$groups <- bind_rows(v$groups, tibble(id = ids, founded = today, tax = FALSE))
  v
}

#' A player's decision to apply, and a group's decision to admit
#'
#' Each player outside a group applies with chance `rules$player$revise`.
#' They pick the group whose members make the most of the other goods they
#' need. The group admits them if it has fewer than `rules$groups$max_same`
#' makers of their good. Applicants are handled in random order, so a place
#' taken by one is gone for the next.
#'
#' @inheritParams event_drought
#'
#' @return The village, with `players$group` updated.
#' @export
players_join <- function(v, rules) {
  p        <- v$players
  n_groups <- nrow(v$groups)
  G        <- ncol(p$eff)
  applicants <- which(is.na(p$group) & p$choice > 0 &
                        runif(length(p$group)) < rules$player$revise)
  if (n_groups == 0 || length(applicants) == 0) return(v)

  m      <- which(!is.na(p$group) & p$choice > 0)
  makers <- matrix(tabulate((p$choice[m] - 1L) * n_groups + p$group[m], n_groups * G),
                   nrow = n_groups)                         # group x good

  for (i in applicants[sample.int(length(applicants))]) {
    mine    <- p$choice[i]
    covered <- rowSums(makers[, -mine, drop = FALSE] > 0)
    open    <- which(makers[, mine] < rules$groups$max_same & covered > 0)
    if (length(open) > 0) {
      g <- open[which.max(covered[open])]
      p$group[i]      <- g
      makers[g, mine] <- makers[g, mine] + 1L
    }
  }

  v$players$group <- p$group
  v
}

#' The membership decisions, in order
#'
#' Returns the decisions [step_membership()] runs: leave, expel, found, join.
#' Each is a function that takes a village and the rules, and returns the
#' village. Change the list to change how groups form.
#'
#' @return A named list of functions.
#' @export
#'
#' @examples
#' # groups that never expel anyone
#' decisions <- membership_decisions()
#' decisions$expel <- NULL
#' names(decisions)
membership_decisions <- function() {
  list(
    leave = members_leave,
    expel = groups_expel,
    found = players_found,
    join  = players_join
  )
}

#' Step: who belongs to which group
#'
#' Runs the membership decisions in order. Does nothing when groups are off.
#'
#' To use different decisions, wrap this step and put the wrapper in the
#' schedule:
#' `schedule$membership <- function(v, rules) step_membership(v, rules, my_decisions)`.
#'
#' @inheritParams event_drought
#' @param decisions A list of decision functions, as from
#'   [membership_decisions()].
#'
#' @return The village.
#' @export
step_membership <- function(v, rules, decisions = membership_decisions()) {
  if (!rules$groups$on) return(v)
  for (decide in decisions) v <- decide(v, rules)
  v
}

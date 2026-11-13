#' Start or end a drought
#'
#' During a drought, one good takes `rules$drought$hours` to make, whoever
#' makes it. Skills are untouched, so the good returns to its usual cost when
#' the drought ends.
#'
#' With `rules$drought$p` at 0, there are two scripted droughts: the first
#' starts on the village's `drought_start` day, the second once
#' `groups_needed` groups have formed. Above 0, a drought starts with that
#' chance on any day without one.
#'
#' @param v A village from [new_village()].
#' @param rules Rules from [set_rules()].
#'
#' @return The village, with `world` updated.
#' @export
event_drought <- function(v, rules) {
  d <- rules$drought
  if (!d$on) return(v)
  w <- v$world

  starts <- w$drought_left == 0 && if (d$p > 0) {
    runif(1) < d$p
  } else {
    (w$droughts_done == 0 && w$day + 1L >= w$drought_start) ||
      (w$droughts_done == 1 && count_groups(v$players$group) >= d$groups_needed)
  }

  if (starts) {
    w$drought_good  <- sample.int(ncol(v$players$eff), 1)
    w$drought_left  <- as.integer(d$length)
    w$droughts_done <- w$droughts_done + 1L
  } else if (w$drought_left == 0) {
    w$drought_good <- NA_integer_
  }

  v$world <- w
  v
}

#' Invent a new product
#'
#' At most once per run, a player invents a cheaper way to meet one need.
#' The inventor, and their group if `share_in_group` is `TRUE`, can make it
#' at `advantage` times the village's average efficiency in the old good.
#' Everyone else learns it through [step_learn()].
#'
#' When droughts are on, the invention waits until `after_droughts` droughts
#' have happened.
#'
#' @inheritParams event_drought
#'
#' @return The village, with `players$new_eff` and `world` updated.
#' @export
event_invention <- function(v, rules) {
  i <- rules$invention
  w <- v$world
  p <- v$players

  ready <- i$on && is.na(w$new_need) && w$drought_left == 0 &&
    (!rules$drought$on || w$droughts_done >= i$after_droughts)
  if (!ready) return(v)

  inventors <- which(runif(nrow(p$eff)) < i$p)
  if (length(inventors) == 0) return(v)

  inventor <- inventors[sample.int(length(inventors), 1)]
  target   <- sample.int(ncol(p$eff), 1)
  team     <- inventor
  if (i$share_in_group && !is.na(p$group[inventor])) {
    team <- which(p$group %in% p$group[inventor])
  }

  v$players$new_eff[team, target] <- i$advantage * mean(p$eff[, target])
  v$world$new_need <- target
  v$world$inventor <- inventor
  v
}

#' Work out today's hours per unit
#'
#' A unit costs `need / efficiency` hours, using the better of a player's old
#' and new ways of making the good. In a drought, the good hit costs
#' `rules$drought$hours` for everyone.
#'
#' Every later step reads the result from `v$today$unit_hours`, so this must
#' run before them.
#'
#' @inheritParams event_drought
#'
#' @return The village, with `today$unit_hours` set.
#' @export
set_unit_hours <- function(v, rules) {
  unit <- rules$world$need / pmax(v$players$eff, v$players$new_eff, na.rm = TRUE)
  if (!is.na(v$world$drought_good)) unit[, v$world$drought_good] <- rules$drought$hours
  v$today$unit_hours <- unit
  v
}

#' Step: the world's events
#'
#' The first step of a day: droughts, the invention, then today's hours per
#' unit. To add an event of your own, write a function that takes and returns
#' a village, and call it from a step placed before or after this one in the
#' schedule.
#'
#' @inheritParams event_drought
#'
#' @return The village.
#' @export
#' @seealso [event_drought()], [event_invention()], [set_unit_hours()]
step_events <- function(v, rules) {
  v |>
    event_drought(rules) |>
    event_invention(rules) |>
    set_unit_hours(rules)
}

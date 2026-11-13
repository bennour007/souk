#' The steps of a day, in order
#'
#' Returns the default schedule: events, membership, specialise, market,
#' learn, invest, govern. Every step is a function that takes a village and
#' the rules, and returns the village.
#'
#' The schedule is how you change the model. Replace a step to change a
#' decision, remove one to switch it off, or add one for something new.
#'
#' @return A named list of step functions.
#' @export
#' @seealso [step_events()], [step_membership()], [step_specialise()],
#'   [step_market()], [step_learn()], [step_invest()], [step_govern()]
#'
#' @examples
#' schedule <- default_schedule()
#' names(schedule)
#'
#' # players who never change specialty
#' schedule$specialise <- function(v, rules) v
default_schedule <- function() {
  list(
    events     = step_events,
    membership = step_membership,
    specialise = step_specialise,
    market     = step_market,
    learn      = step_learn,
    invest     = step_invest,
    govern     = step_govern
  )
}

#' Play one day
#'
#' Empties `v$today`, runs each step of the schedule, checks the books if
#' `rules$world$check` is `TRUE`, and moves the day forward.
#'
#' Each step gets its own random stream, set from the village's seed, the day
#' and the step's position. So the same village and rules always give the
#' same day, and switching one rule on doesn't change the draws of unrelated
#' steps earlier in the day. Your session's random state is restored
#' afterwards.
#'
#' @inheritParams event_drought
#' @param schedule A list of steps, as from [default_schedule()].
#'
#' @return The village after the day, with the day's values still in `today`.
#' @export
#'
#' @examples
#' village <- new_village(n_players = 10, n_goods = 3)
#' village <- play_day(village, set_rules())
#' village$players$free
play_day <- function(v, rules, schedule = default_schedule()) {
  withr::local_preserve_seed()
  v$today <- list()

  for (k in seq_along(schedule)) {
    set.seed(as.integer(v$world$seed * 1e6 + v$world$day * 100 + k))
    v <- schedule[[k]](v, rules)
  }

  if (rules$world$check) check_village(v, rules)
  v$world$drought_left <- max(v$world$drought_left - 1L, 0L)
  v$world$day          <- v$world$day + 1L
  v
}

#' Record the players for the day just played
#'
#' @param v A village returned by [play_day()].
#'
#' @return A tibble with one row per player: `day`, `player`, `choice` (the
#'   good they make, or `"none"`), `group`, `member`, `taxed`, `status`
#'   (`"trading"`, `"alone"` or `"short"`), `free_hours`, `shortfall`,
#'   `alone_hours` (what the day would cost without trade), `swaps`, `inside`
#'   and `outside` (swaps in the group and on the open market), `efficiency`
#'   (in their own good), `knows_new`, `uses_new`, `drought_good`, `new_need`
#'   and `inventor`.
#' @export
record_players <- function(v) {
  p      <- v$players
  t      <- v$today
  w      <- v$world
  goods  <- colnames(p$eff)
  n      <- nrow(p$eff)
  maker  <- p$choice > 0
  column <- pmax(p$choice, 1L)
  need   <- w$new_need

  tibble(
    day          = w$day,
    player       = seq_len(n),
    choice       = if_else(maker, goods[column], "none"),
    group        = p$group,
    member       = if_else(is.na(p$group), "Outside a group", "In a group"),
    taxed        = t$taxed,
    status       = t$status,
    free_hours   = p$free,
    shortfall    = t$shortfall,
    alone_hours  = t$alone,
    swaps        = t$swaps,
    inside       = t$inside,
    outside      = t$outside,
    efficiency   = if_else(maker, p$eff[cbind(seq_len(n), column)], NA_real_),
    knows_new    = if (is.na(need)) FALSE else !is.na(p$new_eff[, need]),
    uses_new     = if (is.na(need)) FALSE else
      p$choice == need & coalesce(p$new_eff[, need] > p$eff[, need], FALSE),
    drought_good = if (is.na(w$drought_good)) NA_character_ else goods[w$drought_good],
    new_need     = if (is.na(need)) NA_character_ else goods[need],
    inventor     = w$inventor
  )
}

#' Record the groups for the day just played
#'
#' @param v A village returned by [play_day()].
#'
#' @return A tibble with one row per group ever founded: `day`, `group`,
#'   `founded`, `members` and `tax`. It has no rows while no group exists.
#' @export
record_groups <- function(v) {
  g <- v$groups
  tibble(
    day     = rep(v$world$day, nrow(g)),
    group   = g$id,
    founded = g$founded,
    members = tabulate(v$players$group[!is.na(v$players$group)], nrow(g)),
    tax     = g$tax
  )
}

#' The default recorders
#'
#' A recorder is a function that takes the village after a day and returns a
#' tibble. [run_village()] calls every recorder after every day and stacks
#' the results. Add one of your own to keep something else, such as trade
#' flows.
#'
#' @return A named list of recorder functions.
#' @export
default_recorders <- function() {
  list(players = record_players, groups = record_groups)
}

#' Play many days and record them
#'
#' @param village A village from [new_village()].
#' @param n_days Number of days to play.
#' @param rules Rules from [set_rules()].
#' @param schedule A list of steps, as from [default_schedule()].
#' @param recorders A named list of recorders, as from [default_recorders()].
#'
#' @return A list with one stacked tibble per recorder (by default `players`
#'   and `groups`), plus `start` and `end`, the village before the first day
#'   and after the last.
#' @export
#'
#' @examples
#' village <- new_village(n_players = 20, n_goods = 4, seed = 3)
#' run     <- run_village(village, n_days = 10)
#' run$players
#'
#' # the same village with groups
#' with_groups <- run_village(
#'   village, n_days = 10,
#'   rules = set_rules(groups = list(on = TRUE, found = 0.2, found_from = 1))
#' )
#' with_groups$groups
run_village <- function(village, n_days = 300, rules = set_rules(),
                        schedule = default_schedule(),
                        recorders = default_recorders()) {
  withr::local_preserve_seed()

  states <- accumulate(seq_len(n_days), \(v, d) play_day(v, rules, schedule),
                       .init = village, .simplify = FALSE)

  records <- map(recorders, \(record) bind_rows(map(states[-1], record)))
  c(records, list(start = village, end = states[[n_days + 1]]))
}

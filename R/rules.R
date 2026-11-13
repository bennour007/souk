#' Set the rules of the village
#'
#' Returns the complete set of rules, with defaults for everything you don't
#' change. Rules are grouped into sets by who or what they concern. To change
#' a rule, pass its set as a named list:
#' `set_rules(groups = list(on = TRUE, found = 0.01))`.
#'
#' A misspelt set or rule name is an error, so a typo can't silently leave a
#' default in place.
#'
#' @param ... Named lists, one per rule set to change. The sets and their
#'   rules are:
#'   \describe{
#'     \item{`world`}{`day_length`: hours in a day. `need`: units of each good
#'       a player needs per day. `min_work`: hours every player works whatever
#'       happens, which caps free hours at `day_length - min_work`. `check`:
#'       run [check_village()] after every day.}
#'     \item{`skills`}{`lambda`: return on a full day of invested hours.
#'       `forget`: daily fading of an unused skill. `e_max`, `e_min`: the
#'       highest and lowest efficiency. `floor_share`: a skill never fades
#'       below this share of where it started.}
#'     \item{`player`}{`revise`: the daily chance that a player reconsiders
#'       their specialty, leaving a group, or applying to one.}
#'     \item{`market`}{`swap_cost`, `swap_cost_group`: hours one swap takes on
#'       the open market and inside a group.}
#'     \item{`groups`}{`on`: whether groups exist. `found`: chance that a
#'       player outside a group founds one, on founding days. `found_every`,
#'       `found_from`: founding happens every so many days, from this day.
#'       `max_same`: most makers of one good a group accepts. `beta`: gain in
#'       efficiency per swap inside the group.}
#'     \item{`tax`}{`on`: whether groups can tax outsiders; needs groups.
#'       `rate`: hours an outsider pays per swap with a member of a taxing
#'       group. `vote_every`: days between votes. `disturbance`: chance that a
#'       member votes against their own calculation. `voting`: `"majority"`,
#'       `"weighted"`, or a function; see [tally_votes()].}
#'     \item{`drought`}{`on`: whether droughts happen. `hours`: hours per unit
#'       of the good hit. `length`: days a drought lasts. `p`: daily chance of
#'       a drought; at 0, there are two scripted droughts, one in the first
#'       days and one once `groups_needed` groups have formed.}
#'     \item{`invention`}{`on`: whether a new product can be invented. `p`:
#'       daily chance per player. `advantage`: how much more efficient the new
#'       product is than the average old way. `after_droughts`: droughts that
#'       must have happened first, when droughts are on. `share_in_group`:
#'       whether the inventor's group learns it at once. `learn_days`:
#'       purchases needed to learn it. `learn_share`: a learner's starting
#'       efficiency, as a share of what knowers have.}
#'   }
#'
#' @return A named list of rule sets, each a named list.
#' @export
#'
#' @examples
#' rules <- set_rules()
#' rules$skills$lambda
#'
#' # groups that can tax, and random droughts
#' set_rules(
#'   groups  = list(on = TRUE),
#'   tax     = list(on = TRUE, rate = 0.25),
#'   drought = list(on = TRUE, p = 0.02)
#' )
set_rules <- function(...) {
  rules <- list(
    world     = list(day_length = 12, need = 1, min_work = 2, check = TRUE),
    skills    = list(lambda = 0.05, forget = 0.01, e_max = 4, e_min = 0.1,
                     floor_share = 0),
    player    = list(revise = 0.2),
    market    = list(swap_cost = 0, swap_cost_group = 0),
    groups    = list(on = FALSE, found = 0.001, found_every = 5, found_from = 10,
                     max_same = 1, beta = 0.001),
    tax       = list(on = FALSE, rate = 0.5, vote_every = 5, disturbance = 0.1,
                     voting = "majority"),
    drought   = list(on = FALSE, hours = 8, length = 3, p = 0, groups_needed = 3),
    invention = list(on = FALSE, p = 0.001, advantage = 3, after_droughts = 2,
                     share_in_group = TRUE, learn_days = 10, learn_share = 0.5)
  )

  changes <- list(...)
  unknown <- setdiff(names(changes), names(rules))
  if (length(changes) > 0 && (is.null(names(changes)) || length(unknown) > 0)) {
    stop("Unknown rule set: ", toString(unknown),
         ". Sets are: ", toString(names(rules)), ".", call. = FALSE)
  }

  for (set in names(changes)) {
    if (!is.list(changes[[set]])) {
      stop("Rules for '", set, "' must be a named list.", call. = FALSE)
    }
    unknown <- setdiff(names(changes[[set]]), names(rules[[set]]))
    if (length(unknown) > 0) {
      stop("Unknown rule in '", set, "': ", toString(unknown), ".", call. = FALSE)
    }
    rules[[set]] <- modifyList(rules[[set]], changes[[set]])
  }

  check_rules(rules)
}

# rules that contradict each other, or values that can't work
check_rules <- function(rules) {
  chances <- c(
    revise = rules$player$revise, found = rules$groups$found,
    disturbance = rules$tax$disturbance, drought = rules$drought$p,
    invention = rules$invention$p, forget = rules$skills$forget
  )
  bad <- names(chances)[chances < 0 | chances > 1]
  if (length(bad) > 0) {
    stop("These must be between 0 and 1: ", toString(bad), ".", call. = FALSE)
  }
  if (rules$tax$on && !rules$groups$on) {
    stop("Taxes need groups: set groups = list(on = TRUE).", call. = FALSE)
  }
  if (rules$skills$e_min >= rules$skills$e_max) {
    stop("`e_min` must be below `e_max`.", call. = FALSE)
  }
  if (rules$world$min_work >= rules$world$day_length) {
    stop("`min_work` must be below `day_length`.", call. = FALSE)
  }
  rules
}

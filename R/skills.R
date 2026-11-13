#' Step: buyers learn the new product
#'
#' Does nothing until a product has been invented. After that, each time a
#' player swaps for the need it meets, their exposure grows by the share of
#' that good's suppliers who make it the new way. At `learn_days` of
#' exposure, they learn it, starting at `learn_share` of the efficiency of
#' those who already know it.
#'
#' @inheritParams event_drought
#'
#' @return The village, with `players$exposure` and `players$new_eff` updated.
#' @export
step_learn <- function(v, rules) {
  t <- v$world$new_need
  if (is.na(t)) return(v)

  p       <- v$players
  matched <- v$today$matched
  knows   <- !is.na(p$new_eff[, t])
  level   <- rules$invention$learn_share * mean(p$new_eff[, t], na.rm = TRUE)

  suppliers <- p$choice == t
  sold      <- matched[suppliers, , drop = FALSE]
  new_share <- colSums(sold & knows[suppliers]) / pmax(colSums(sold), 1)

  p$exposure <- p$exposure + matched[, t] * new_share[pmax(p$choice, 1L)]
  p$new_eff[!knows & p$exposure >= rules$invention$learn_days, t] <- level

  v$players <- p
  v
}

#' Step: players invest their free hours
#'
#' Free hours are spread over the goods a player worked on, in proportion to
#' the hours each took, and raise efficiency in them. Growth slows as a skill
#' nears its ceiling, `e_max`, so nobody automates their work away:
#'
#' `growth = 1 + (1 - skill / ceiling) * (lambda * invested / day_length + beta * swaps inside the group)`
#'
#' The group bonus applies to the player's own good only. A skill not used
#' today fades by `forget`, except the good in drought, and never below the
#' larger of `e_min` and `floor_share` times its starting level. A player who
#' makes a good the new way improves at that, up to `e_max * advantage`,
#' while their old way fades.
#'
#' @inheritParams event_drought
#'
#' @return The village, with `players$eff` and `players$new_eff` updated, and
#'   `today$invested` set.
#' @export
step_invest <- function(v, rules) {
  p  <- v$players
  k  <- rules$skills
  E  <- p$eff
  NE <- p$new_eff
  H  <- v$today$hours
  n  <- nrow(E)
  G  <- ncol(E)

  invested <- p$free * H / rowSums(H)
  uses_new <- !is.na(NE) & NE > E
  skill    <- ifelse(uses_new, NE, E)
  ceiling  <- ifelse(uses_new, k$e_max * rules$invention$advantage, k$e_max)
  room     <- pmax(1 - skill / ceiling, 0)

  own <- matrix(FALSE, n, G)
  own[cbind(which(p$choice > 0), p$choice[p$choice > 0])] <- TRUE

  growth <- 1 + room * (k$lambda * invested / rules$world$day_length +
                          rules$groups$beta * v$today$inside * own)

  unused <- H == 0
  if (!is.na(v$world$drought_good)) unused[, v$world$drought_good] <- FALSE

  floor <- pmax(k$floor_share * p$eff_start, k$e_min)
  new   <- ifelse(uses_new, E * (1 - k$forget),
                  E * growth * ifelse(unused, 1 - k$forget, 1))

  p$new_eff <- ifelse(uses_new, NE * growth, NE)
  p$eff     <- pmax(new, floor)
  dimnames(p$eff) <- dimnames(E)

  v$players        <- p
  v$today$invested <- invested
  v
}

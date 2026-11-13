#' The rounds of the market
#'
#' A round puts every player in a segment, and swaps only happen within a
#' segment. By default there is a `group` round, where the segment is the
#' player's group and players outside a group sit out, followed by an `open`
#' round with everyone in one segment.
#'
#' This is where a network enters the model. To add local markets, return an
#' extra round whose segment is each player's community, and pass your
#' function to [step_market()]. Rounds other than `group` count as the open
#' market.
#'
#' @inheritParams event_drought
#'
#' @return A named list of integer vectors, one per round, each with one
#'   segment per player; `NA` means the player sits the round out.
#' @export
market_rounds <- function(v, rules) {
  rounds <- list(open = rep(1L, length(v$players$choice)))
  if (rules$groups$on) rounds <- c(list(group = v$players$group), rounds)
  rounds
}

#' Match swaps within segments
#'
#' Within each segment, makers of good `k` who want `j` swap one for one with
#' makers of `j` who want `k`. The scarcer side sets how many swaps happen,
#' and who gets them is random, except that players marked `last` are served
#' after everyone else.
#'
#' @param choice The good each player makes, as a column number; 0 for none.
#' @param wants A players-by-goods logical matrix: `TRUE` where the player
#'   still needs to get that good by swapping.
#' @param segment Each player's segment; `NA` sits out.
#' @param last Logical, one per player: served last within their segment.
#'
#' @return A logical matrix shaped like `wants`: `TRUE` where the swap
#'   happened. For every swap of `k` for `j` there is one of `j` for `k`.
#' @export
#'
#' @examples
#' # two makers of good 1 and one of good 2: only one swap is possible
#' choice <- c(1, 1, 2)
#' wants  <- rbind(c(FALSE, TRUE), c(FALSE, TRUE), c(TRUE, FALSE))
#' match_round(choice, wants, segment = c(1, 1, 1))
match_round <- function(choice, wants, segment, last = rep(FALSE, length(choice))) {
  n <- nrow(wants)
  G <- ncol(wants)
  matched <- matrix(FALSE, n, G)

  active <- which(choice > 0 & !is.na(segment))
  rows   <- which(wants[active, , drop = FALSE], arr.ind = TRUE)
  if (nrow(rows) == 0) return(matched)

  seg <- match(segment[active], unique(segment[active]))
  S   <- max(seg)
  i   <- active[rows[, 1]]                                  # the player
  j   <- rows[, 2]                                          # the good they want
  k   <- choice[i]                                          # the good they make
  s   <- seg[rows[, 1]]

  key   <- s + S * (k - 1L) + S * G * (j - 1L)
  left  <- array(tabulate(key, S * G * G), c(S, G, G))      # segment x makes x wants
  quota <- pmin(left, aperm(left, c(1, 3, 2)))
  place <- ave(runif(length(i)) + last[i], key, FUN = rank)
  ok    <- place <= quota[cbind(s, k, j)]

  matched[cbind(i[ok], j[ok])] <- TRUE
  matched
}

#' Step: the market
#'
#' Players swap, round by round, and the day's hours are counted.
#'
#' 1. Every maker wants one unit of every good but their own.
#' 2. Rounds run in order, each on the wants still open; see
#'    [market_rounds()]. In open rounds, members of taxing groups are served
#'    last.
#' 3. An outsider pays `rules$tax$rate` hours for each open-market swap, times
#'    the share of the suppliers of that good who belong to a taxing group.
#'    Members of taxing groups receive the same hours.
#' 4. A player makes their own unit plus one per swap, and makes whatever
#'    they couldn't swap themselves. Each swap also takes `swap_cost` hours,
#'    or `swap_cost_group` inside a group.
#' 5. Free hours are what is left of the day, between 0 and
#'    `day_length - min_work`. A player short of hours has a short day.
#'
#' Results go to `v$today` (`hours`, `matched`, `swaps`, `inside`, `outside`,
#' `taxed`, `tax_paid`, `tax_received`, `alone`, `shortfall`, `status`,
#' `sell`) and free hours to `v$players$free`.
#'
#' @inheritParams event_drought
#' @param rounds A function that returns the rounds, like [market_rounds()].
#'
#' @return The village.
#' @export
step_market <- function(v, rules, rounds = market_rounds) {
  p      <- v$players
  U      <- v$today$unit_hours
  n      <- nrow(U)
  G      <- ncol(U)
  choice <- p$choice
  maker  <- choice > 0
  own    <- cbind(which(maker), choice[maker])
  taxed  <- rules$tax$on & coalesce(v$groups$tax[p$group], FALSE)
  rate   <- if (rules$tax$on) rules$tax$rate else 0

  wants      <- matrix(maker, n, G)
  wants[own] <- FALSE

  inside  <- matrix(FALSE, n, G)
  outside <- matrix(FALSE, n, G)
  asked   <- numeric(n)                                     # wants taken to open rounds
  segments <- rounds(v, rules)

  for (round in names(segments)) {
    if (round == "group") {
      got    <- match_round(choice, wants, segments[[round]])
      inside <- inside | got
    } else {
      asked   <- asked + rowSums(wants)
      got     <- match_round(choice, wants, segments[[round]], last = taxed)
      outside <- outside | got
    }
    wants <- wants & !got
  }

  # taxes: the share of a player's open-market suppliers in a taxing group
  taxed_share <- matrix(0, G, G)
  for (k in seq_len(G)) {
    sold <- outside[choice == k, , drop = FALSE]
    taxed_share[k, ] <- colSums(sold & taxed[choice == k]) / pmax(colSums(sold), 1)
  }
  partner_taxed <- t(taxed_share)[pmax(choice, 1L), , drop = FALSE]
  tax_paid      <- rate * rowSums(outside * partner_taxed)
  tax_received  <- rate * taxed * rowSums(outside)

  matched        <- inside | outside
  swaps          <- rowSums(matched)
  hours          <- U
  hours[matched] <- 0
  hours[own]     <- (1 + swaps[maker]) * U[own]

  n_inside  <- rowSums(inside)
  n_outside <- rowSums(outside)
  swapping  <- rules$market$swap_cost * n_outside +
    rules$market$swap_cost_group * n_inside
  dues     <- rules$groups$dues * !is.na(p$group)           # members give time to the group
  balance  <- rules$world$day_length - rowSums(hours) - swapping - dues -
    tax_paid + tax_received

  v$today <- c(v$today, list(
    hours        = hours,
    matched      = matched,
    swaps        = swaps,
    inside       = n_inside,
    outside      = n_outside,
    taxed        = taxed,
    tax_paid     = tax_paid,
    tax_received = tax_received,
    alone        = rowSums(U),
    shortfall    = pmax(-balance, 0),
    status       = case_when(balance < 0 ~ "short", swaps > 0 ~ "trading",
                             .default = "alone"),
    sell         = vapply(seq_len(G), \(k) sum(n_outside[choice == k]) /
                            sum(asked[choice == k]), numeric(1))
  ))
  v$players$free <- pmin(pmax(balance, 0),
                         rules$world$day_length - rules$world$min_work)
  v
}

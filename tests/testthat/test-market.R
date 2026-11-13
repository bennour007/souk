test_that("the scarcer side limits swaps", {
  choice  <- c(1, 1, 2)
  wants   <- rbind(c(FALSE, TRUE), c(FALSE, TRUE), c(TRUE, FALSE))
  matched <- match_round(choice, wants, segment = c(1, 1, 1))
  expect_equal(sum(matched[1:2, 2]), 1)
  expect_true(matched[3, 1])
})

test_that("swaps stay inside segments", {
  choice  <- c(1, 2, 1, 2)
  wants   <- rbind(c(FALSE, TRUE), c(TRUE, FALSE), c(FALSE, TRUE), c(TRUE, FALSE))
  matched <- match_round(choice, wants, segment = c(1, 1, 2, NA))
  expect_true(matched[1, 2] && matched[2, 1])
  expect_false(any(matched[3:4, ]))
})

test_that("players marked last are served last", {
  choice <- c(1, 1, 2)
  wants  <- rbind(c(FALSE, TRUE), c(FALSE, TRUE), c(TRUE, FALSE))
  for (i in 1:10) {
    matched <- match_round(choice, wants, c(1, 1, 1), last = c(TRUE, FALSE, FALSE))
    expect_true(matched[2, 2])
    expect_false(matched[1, 2])
  }
})

test_that("two traders swap and keep 10 free hours", {
  rules <- set_rules()
  v     <- to_market(two_traders(), rules)
  expect_equal(v$today$swaps, c(1, 1))
  expect_equal(v$players$free, c(10, 10))       # 12 hours minus two units at 1 hour
  expect_equal(v$today$status, c("trading", "trading"))
  expect_no_error(check_village(v, rules))
})

test_that("swapping takes time when it has a cost", {
  rules <- set_rules(market = list(swap_cost = 1))
  expect_equal(to_market(two_traders(), rules)$players$free, c(9, 9))
})

test_that("players who make the same good make everything themselves", {
  rules <- set_rules()
  v     <- two_traders()
  v$players$eff[]  <- c(1, 1, 0.5, 0.5)         # both: (1, 0.5)
  v$players$choice <- c(1L, 1L)
  v <- to_market(v, rules)
  expect_equal(v$today$swaps, c(0, 0))
  expect_equal(v$players$free, c(9, 9))         # 12 minus 1 hour minus 2 hours
  expect_equal(v$today$status, c("alone", "alone"))
})

test_that("an outsider pays the tax to a member of a taxing group", {
  rules <- set_rules(groups = list(on = TRUE), tax = list(on = TRUE, rate = 0.5))
  v     <- two_traders()
  v$groups        <- tibble::tibble(id = 1L, founded = 1L, tax = TRUE)
  v$players$group <- c(1L, NA)
  v <- to_market(v, rules)
  expect_equal(v$today$tax_paid, c(0, 0.5))
  expect_equal(v$today$tax_received, c(0.5, 0))
  expect_equal(v$players$free, c(10, 9.5))      # the member is capped at 10
  expect_no_error(check_village(v, rules))
})

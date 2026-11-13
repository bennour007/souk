invest_day <- function(v, rules) to_market(v, rules) |> step_invest(rules)

test_that("a used skill grows and an unused one fades", {
  eff <- invest_day(two_traders(), set_rules())$players$eff
  expect_gt(eff[[1, 1]], 1)
  expect_equal(eff[[1, 2]], 0.5 * 0.99)
})

test_that("skills stop at the ceiling", {
  v <- two_traders()
  v$players$eff[1, 1] <- 4
  expect_equal(invest_day(v, set_rules())$players$eff[[1, 1]], 4)
})

test_that("skills don't fade below the floor", {
  rules <- set_rules(skills = list(floor_share = 1))
  expect_equal(invest_day(two_traders(), rules)$players$eff[[1, 2]], 0.5)
})

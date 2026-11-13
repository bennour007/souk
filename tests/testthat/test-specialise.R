test_that("the scarcer side sets the chance of a swap", {
  chance <- swap_chances(c(1, 1, 2), n_goods = 2)
  expect_equal(chance, matrix(c(0, 1, 0.5, 0), 2))
})

test_that("option costs match a hand calculation", {
  unit_hours <- matrix(c(2, 4), nrow = 1)
  costs <- option_costs(unit_hours, swap_chances(c(1, 2), 2))
  expect_equal(unname(costs[1, ]), c(4, 8, 6))    # make A, make B, alone

  with_cost <- option_costs(unit_hours, swap_chances(c(1, 2), 2), swap_cost = 1)
  expect_equal(unname(with_cost[1, ]), c(5, 9, 6))
})

test_that("nobody switches when nobody reconsiders", {
  rules <- set_rules(player = list(revise = 0))
  v     <- new_village(10, 3) |> step_events(rules)
  expect_equal(step_specialise(v, rules)$players$choice, v$players$choice)
})

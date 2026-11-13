test_that("defaults are returned and can be changed", {
  expect_equal(set_rules()$world$day_length, 12)
  rules <- set_rules(skills = list(lambda = 0.1))
  expect_equal(rules$skills$lambda, 0.1)
  expect_equal(rules$skills$forget, 0.01)
})

test_that("typos are errors", {
  expect_error(set_rules(skils = list(lambda = 0.1)), "Unknown rule set")
  expect_error(set_rules(skills = list(lamda = 0.1)), "Unknown rule in 'skills'")
  expect_error(set_rules(skills = 0.1), "named list")
})

test_that("contradictory rules are errors", {
  expect_error(set_rules(tax = list(on = TRUE)), "Taxes need groups")
  expect_error(set_rules(player = list(revise = 2)), "between 0 and 1")
})

everything <- set_rules(
  groups    = list(on = TRUE, found = 0.2, found_from = 2, found_every = 1),
  tax       = list(on = TRUE),
  drought   = list(on = TRUE, groups_needed = 1),
  invention = list(on = TRUE, p = 0.05, after_droughts = 1),
  market    = list(swap_cost = 0.25, swap_cost_group = 0.1)
)

test_that("a run records every player every day", {
  run <- run_village(new_village(10, 3), n_days = 5)
  expect_equal(nrow(run$players), 50)
  expect_equal(range(run$players$day), c(1, 5))
  expect_equal(run$end$world$day, 5)
})

test_that("the same village and rules give the same run", {
  v <- new_village(15, 3, seed = 7)
  expect_equal(run_village(v, 10, everything), run_village(v, 10, everything))
})

test_that("a run leaves the session's random numbers alone", {
  set.seed(42)
  before <- runif(1)
  set.seed(42)
  run_village(new_village(10, 3), n_days = 3)
  expect_equal(runif(1), before)
})

test_that("the first drought is the same with and without groups", {
  v      <- new_village(20, 4, seed = 2)
  first  <- function(run) {
    hit <- run$players[!is.na(run$players$drought_good), ]
    c(min(hit$day), hit$drought_good[1])
  }
  plain  <- run_village(v, 12, set_rules(drought = list(on = TRUE)))
  groups <- run_village(v, 12, set_rules(drought = list(on = TRUE),
                                         groups  = list(on = TRUE, found = 0.5,
                                                        found_from = 1, found_every = 1)))
  expect_equal(first(plain), first(groups))
})

test_that("the books balance with every rule on", {
  expect_no_error(run_village(new_village(30, 4, seed = 5), 40, everything))
})

test_that("the schedule can be changed", {
  schedule <- default_schedule()
  schedule$specialise <- function(v, rules) v
  v   <- new_village(10, 3)
  run <- run_village(v, 5, schedule = schedule)
  expect_equal(run$end$players$choice, v$players$choice)
})

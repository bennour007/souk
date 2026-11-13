found_now <- set_rules(groups = list(on = TRUE, found = 1, found_from = 1,
                                     found_every = 1))

test_that("players found groups on founding days only", {
  v <- players_found(new_village(5, 3), found_now)
  expect_equal(nrow(v$groups), 5)
  expect_equal(sort(v$players$group), 1:5)

  later <- set_rules(groups = list(on = TRUE, found = 1, found_from = 10))
  expect_equal(nrow(players_found(new_village(5, 3), later)$groups), 0)
})

test_that("members who do worse than outsiders leave", {
  rules <- set_rules(groups = list(on = TRUE), player = list(revise = 1))
  v     <- new_village(3, 2)
  v$players$group <- c(1L, 1L, NA)
  v$players$free  <- c(2, 9, 5)
  expect_equal(members_leave(v, rules)$players$group, c(NA, 1L, NA))
})

test_that("a group expels its surplus makers and keeps the best-off", {
  rules <- set_rules(groups = list(on = TRUE, max_same = 1))
  v     <- new_village(3, 2)
  v$groups         <- tibble::tibble(id = 1L, founded = 1L, tax = FALSE)
  v$players$choice <- c(1L, 1L, 2L)
  v$players$group  <- c(1L, 1L, 1L)
  v$players$free   <- c(5, 8, 3)
  expect_equal(groups_expel(v, rules)$players$group, c(NA, 1L, 1L))
})

test_that("a group admits one maker per good", {
  rules <- set_rules(groups = list(on = TRUE, max_same = 1), player = list(revise = 1))
  v     <- new_village(3, 2)
  v$groups         <- tibble::tibble(id = 1L, founded = 1L, tax = FALSE)
  v$players$choice <- c(1L, 2L, 2L)
  v$players$group  <- c(1L, NA, NA)
  joined <- players_join(v, rules)$players$group
  expect_equal(sum(joined == 1, na.rm = TRUE), 2)
})

test_that("membership does nothing when groups are off", {
  v <- new_village(5, 3)
  expect_equal(step_membership(v, set_rules()), v)
})

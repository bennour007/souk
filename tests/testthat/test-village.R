test_that("a village has the right shape", {
  v <- new_village(n_players = 7, n_goods = 3)
  expect_equal(dim(v$players$eff), c(7, 3))
  expect_equal(v$players$choice, max.col(v$players$eff, ties.method = "first"))
  expect_true(all(is.na(v$players$group)))
  expect_equal(nrow(v$groups), 0)
})

test_that("the seed fixes the village", {
  expect_equal(new_village(seed = 4), new_village(seed = 4))
  expect_false(identical(new_village(seed = 4)$players$eff,
                         new_village(seed = 5)$players$eff))
  expect_error(new_village(seed = 5000))
})

test_that("groups are counted by size", {
  expect_equal(count_groups(c(1, 1, 2, NA)), 1)
  expect_equal(count_groups(c(1, 1, 2, NA), min_members = 1), 2)
})

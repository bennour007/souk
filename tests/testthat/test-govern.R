test_that("votes are counted by the chosen rule", {
  yes <- c(TRUE, TRUE, FALSE)
  expect_true(tally_votes(yes, voting = "majority"))
  expect_false(tally_votes(yes, weight = c(1, 1, 10), voting = "weighted"))
  expect_false(tally_votes(yes, voting = function(yes, weight) all(yes)))
  expect_error(tally_votes(yes, voting = "lottery"), "Unknown voting rule")
})

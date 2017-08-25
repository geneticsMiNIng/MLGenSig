data_e <- data.frame(AANAT = c(9, 2, 11, 0, 1),
  AARSD1 = c(2354, 1846, 3391, 2169, 2273),
  AATF = c(2870, 5656, 9522, 4625, 3473),
  AATK = c(317, 312, 736, 169, 92),
  ABCA5 = c(1071, 2107, 1600, 615, 249))
cond <- c("A", "B", "A", "B", "B")

test_that("Input", {
  expect_error(test_edger())
  expect_error((test_edger(data = data_e)))
  expect_error(test_edger(condition = cond))
})

test_that("Output", {
  expect_is(test_edger(data_e, cond), "data.frame")
  expect_equal(nrow(test_edger(data_e, cond)), 5)
  expect_equal(ncol(test_edger(data_e, cond)), 3)
})

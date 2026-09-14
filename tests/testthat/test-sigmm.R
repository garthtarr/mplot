test_that("sigMM returns 1 for adaptive fence", {
  result <- mplot:::sigMM(k.mod = 5, method = "ML", k.full = 10, adaptive = TRUE)

  expect_equal(result, 1)
  expect_length(result, 1)
})

test_that("sigMM calculates standard deviation for non-adaptive fence", {
  result <- mplot:::sigMM(k.mod = 5, method = "ML", k.full = 10, adaptive = FALSE)

  expected <- sqrt((10 - 5) / 2)
  expect_equal(result, expected)
})

test_that("sigMM increases with difference between k.full and k.mod", {
  sig_small <- mplot:::sigMM(k.mod = 8, method = "ML", k.full = 10, adaptive = FALSE)
  sig_large <- mplot:::sigMM(k.mod = 2, method = "ML", k.full = 10, adaptive = FALSE)

  expect_gt(sig_large, sig_small)
})

test_that("sigMM is zero when k.full equals k.mod in non-adaptive case", {
  result <- mplot:::sigMM(k.mod = 10, method = "ML", k.full = 10, adaptive = FALSE)

  expect_equal(result, 0)
})

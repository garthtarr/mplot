test_that("af works with basic linear model and single core", {
  set.seed(123)
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)

  lm_fit <- lm(y ~ ., data = dat)
  result <- af(lm_fit, cores = 1, B = 5, n.c = 5, seed = 123)

  expect_s3_class(result, "af")
  expect_type(result, "list")
  expect_named(result, c("bestOnly", "all", "call", "screen", "k.range"))
})

test_that("af returns non-null bestOnly and all model summaries", {
  set.seed(456)
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)

  lm_fit <- lm(y ~ ., data = dat)
  result <- af(lm_fit, cores = 1, B = 5, n.c = 5, seed = 456)

  expect_false(is.null(result$bestOnly))
  expect_false(is.null(result$all))
})

test_that("af works with multiple cores", {
  set.seed(789)
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)

  lm_fit <- lm(y ~ ., data = dat)
  result <- af(lm_fit, cores = 2, B = 5, n.c = 5, seed = 789)

  expect_s3_class(result, "af")
})

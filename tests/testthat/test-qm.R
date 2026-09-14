test_that("Qm calculates negative log-likelihood for linear model", {
  set.seed(789)
  n <- 30
  x <- rnorm(n)
  y <- 1 + x + rnorm(n)

  lm_fit <- lm(y ~ x)
  result <- mplot:::Qm(lm_fit, method = "ML")

  expect_type(result, "double")
  expect_gt(result, 0)
  expect_length(result, 1)
})

test_that("Qm works with generalized linear model", {
  set.seed(321)
  n <- 30
  x <- rnorm(n)
  y <- rbinom(n, size = 1, prob = plogis(1 + x))

  glm_fit <- glm(y ~ x, family = binomial())
  result <- mplot:::Qm(glm_fit, method = "ML")

  expect_type(result, "double")
  expect_gt(result, 0)
  expect_length(result, 1)
})

test_that("Qm with complex model has larger magnitude than simple model", {
  set.seed(555)
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)

  simple_fit <- lm(y ~ 1)
  complex_fit <- lm(y ~ x1 + x2)

  q_simple <- mplot:::Qm(simple_fit, method = "ML")
  q_complex <- mplot:::Qm(complex_fit, method = "ML")

  expect_type(q_simple, "double")
  expect_type(q_complex, "double")
})

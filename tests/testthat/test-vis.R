test_that("vis returns an object of class vis", {
  set.seed(123)
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2, x3)

  lm_fit <- lm(y ~ ., data = dat)
  result <- vis(lm_fit, cores = 1, B = 5, seed = 123)

  expect_s3_class(result, "vis")
  expect_type(result, "list")
})

test_that("vis result contains expected components", {
  set.seed(456)
  n <- 40
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)

  lm_fit <- lm(y ~ ., data = dat)
  result <- vis(lm_fit, cores = 1, B = 5, seed = 456)

  expect_true(!is.null(result$res.df))
  expect_true(!is.null(result$var.in))
  expect_true(!is.null(result$res.single.pass))
})

test_that("vis works with glm models", {
  set.seed(789)
  n <- 40
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- rbinom(n, size = 1, prob = plogis(1 + x1 + x2))
  dat <- data.frame(y, x1, x2)

  glm_fit <- glm(y ~ ., data = dat, family = binomial())
  result <- vis(glm_fit, cores = 1, B = 5, seed = 789)

  expect_s3_class(result, "vis")
  expect_true(!is.null(result$res.df))
})

test_that("vis handles forced variables correctly", {
  set.seed(222)
  n <- 40
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2, x3)

  lm_fit <- lm(y ~ ., data = dat)
  result <- vis(lm_fit, cores = 1, B = 5, force.in = "x1", seed = 222)

  expect_s3_class(result, "vis")
})

test_that("vis works with multiple cores", {
  set.seed(333)
  n <- 40
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)

  lm_fit <- lm(y ~ ., data = dat)
  result <- vis(lm_fit, cores = 2, B = 5, seed = 333)

  expect_s3_class(result, "vis")
})

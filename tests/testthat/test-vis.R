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

test_that("vis produces reproducible results with same seed", {
  n <- 40
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)

  lm_fit <- lm(y ~ ., data = dat)
  result1 <- vis(lm_fit, cores = 1, B = 5, seed = 8021)
  result2 <- vis(lm_fit, cores = 1, B = 5, seed = 8021)

  expect_identical(result1$res.df, result2$res.df)
  expect_identical(result1$var.in, result2$var.in)
})

test_that("vis produces reproducible results across core counts", {
  n <- 40
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)

  lm_fit <- lm(y ~ ., data = dat)
  result_seq <- vis(lm_fit, cores = 1, B = 5, seed = 6142)
  result_par <- vis(lm_fit, cores = 2, B = 5, seed = 6142)

  expect_identical(result_seq$res.df, result_par$res.df)
  expect_identical(result_seq$var.in, result_par$var.in)
})

test_that("vis restores the caller's future plan on exit", {
  n <- 40
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)
  lm_fit <- lm(y ~ ., data = dat)

  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)

  plan_before <- future::plan()
  vis(lm_fit, cores = 1, B = 5, seed = 111)
  plan_after <- future::plan()

  expect_identical(class(plan_before), class(plan_after))
})

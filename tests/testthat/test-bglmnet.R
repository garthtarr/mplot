# Tests for bglmnet function

test_that("bglmnet function exists and is callable", {
  expect_true(is.function(bglmnet))
})

test_that("bglmnet function has expected parameters", {
  # Verify the function signature
  params <- names(formals(bglmnet))

  expect_true("mf" %in% params)
  expect_true("nlambda" %in% params)
  expect_true("lambda" %in% params)
  expect_true("B" %in% params)
  expect_true("cores" %in% params)
  expect_true("seed" %in% params)
})

test_that("bglmnet runs without error on simple case", {
  set.seed(1)
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)
  lm_fit <- lm(y ~ ., data = dat)

  result <- bglmnet(lm_fit, cores = 1, B = 5, seed = 1)

  expect_s3_class(result, "bglmnet")
  expect_type(result, "list")
  expect_named(
    result,
    c("frequency", "lambda", "mods", "mod.sum", "screen", "vars", "call")
  )
})

test_that("bglmnet works with multiple cores", {
  set.seed(22)
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)
  lm_fit <- lm(y ~ ., data = dat)

  result <- bglmnet(lm_fit, cores = 2, B = 5, seed = 22)

  expect_s3_class(result, "bglmnet")
})

test_that("bglmnet produces reproducible results with same seed", {
  set.seed(4827)
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)
  lm_fit <- lm(y ~ ., data = dat)

  result1 <- bglmnet(lm_fit, cores = 1, B = 8, seed = 4827)
  result2 <- bglmnet(lm_fit, cores = 1, B = 8, seed = 4827)

  expect_identical(result1$frequency, result2$frequency)
  expect_identical(result1$mod.sum, result2$mod.sum)
})

test_that("bglmnet produces reproducible results across core counts", {
  set.seed(5931)
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)
  lm_fit <- lm(y ~ ., data = dat)

  result_seq <- bglmnet(lm_fit, cores = 1, B = 8, seed = 5931)
  result_par <- bglmnet(lm_fit, cores = 2, B = 8, seed = 5931)

  expect_identical(result_seq$frequency, result_par$frequency)
  expect_identical(result_seq$mod.sum, result_par$mod.sum)
})

test_that("bglmnet handles bootstrap reps with equal unique-model counts (#14 regression)", {
  # Regression test for a bug where apply(betaboot, 3, get_unique_mods)
  # silently simplified to an array (instead of a list) whenever every
  # bootstrap replication produced the same number of unique selected
  # models, causing the subsequent do.call(rbind, .) to error with
  # "second argument must be a list". seed = 2 with cores = 1 (and
  # seed = 22 with cores = 2) are known to trigger this configuration.
  set.seed(2)
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)
  lm_fit <- lm(y ~ ., data = dat)

  result <- bglmnet(lm_fit, cores = 1, B = 5, seed = 2)

  expect_s3_class(result, "bglmnet")
  expect_s3_class(result$mod.sum, "data.frame")
  expect_true(nrow(result$mod.sum) > 0)
  # probabilities of models of a given size should sum to 1
  prob_by_k <- tapply(result$mod.sum$prob, result$mod.sum$k, sum)
  expect_equal(as.numeric(prob_by_k), rep(1, length(prob_by_k)))

  set.seed(22)
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)
  lm_fit <- lm(y ~ ., data = dat)

  result_parallel <- bglmnet(lm_fit, cores = 2, B = 5, seed = 22)
  expect_s3_class(result_parallel, "bglmnet")
})

test_that("bglmnet restores the caller's future plan on exit", {
  set.seed(111)
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)
  lm_fit <- lm(y ~ ., data = dat)

  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)

  plan_before <- future::plan()
  bglmnet(lm_fit, cores = 1, B = 5, seed = 111)
  plan_after <- future::plan()

  expect_identical(class(plan_before), class(plan_after))
})

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

test_that("af produces reproducible results with same seed", {
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)

  lm_fit <- lm(y ~ ., data = dat)
  result1 <- af(lm_fit, cores = 1, B = 5, n.c = 5, seed = 4827)
  result2 <- af(lm_fit, cores = 1, B = 5, n.c = 5, seed = 4827)

  expect_identical(result1$bestOnly$p.star, result2$bestOnly$p.star)
  expect_identical(result1$all$p.star, result2$all$p.star)
})

test_that("af produces reproducible results across core counts", {
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)

  lm_fit <- lm(y ~ ., data = dat)
  result_seq <- af(lm_fit, cores = 1, B = 5, n.c = 5, seed = 5931)
  result_par <- af(lm_fit, cores = 2, B = 5, n.c = 5, seed = 5931)

  expect_identical(result_seq$bestOnly$p.star, result_par$bestOnly$p.star)
  expect_identical(result_seq$all$p.star, result_par$all$p.star)
})

test_that("af's bootstrap dispatch order is shuffled but reproducible (#15)", {
  # Regression test for #15: af() randomizes the order in which c.range
  # values are dispatched to future_map(), to avoid furrr's default
  # contiguous chunking systematically clustering cheap/expensive tasks
  # together (see PARALLELIZATION.md). The randomized order is recorded
  # as an internal (undocumented) "dispatch_order" attribute for testing.
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)
  lm_fit <- lm(y ~ ., data = dat)

  n.c <- 10
  result1 <- af(lm_fit, cores = 1, B = 3, n.c = n.c, seed = 7331)
  result2 <- af(lm_fit, cores = 1, B = 3, n.c = n.c, seed = 7331)
  result3 <- af(lm_fit, cores = 1, B = 3, n.c = n.c, seed = 4242)

  order1 <- attr(result1, "dispatch_order")
  order2 <- attr(result2, "dispatch_order")
  order3 <- attr(result3, "dispatch_order")

  # It's a full permutation of the task indices...
  expect_setequal(order1, seq_len(n.c))
  # ...that is actually shuffled, not left in ascending order...
  expect_false(identical(order1, seq_len(n.c)))
  # ...deterministically, for a given seed...
  expect_identical(order1, order2)
  # ...but varies across seeds.
  expect_false(identical(order1, order3))
})

test_that("af restores the caller's future plan on exit", {
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + x1 + x2 + rnorm(n)
  dat <- data.frame(y, x1, x2)
  lm_fit <- lm(y ~ ., data = dat)

  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)

  plan_before <- future::plan()
  af(lm_fit, cores = 1, B = 5, n.c = 5, seed = 111)
  plan_after <- future::plan()

  expect_identical(class(plan_before), class(plan_after))
})

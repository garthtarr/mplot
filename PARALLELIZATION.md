# Parallelization Review & Improvement Plan: mplot Package

## Progress

Tracked in GitHub issues [#11](https://github.com/garthtarr/mplot/issues/11)–[#16](https://github.com/garthtarr/mplot/issues/16); check them off there as they're resolved.

| Priority | Item | Status | Issue / Commit |
|----------|------|--------|-----------------|
| P1 | Fix duplicate cluster closure in `af()` / `vis()` | ✅ Done | `8793a21` |
| P2 | Migrate `af()` to `future`/`furrr`; remove global (`<<-`) assignment | Open | [#11](https://github.com/garthtarr/mplot/issues/11) |
| P2 | Migrate `vis()` to `future`/`furrr`; remove global assignment; fix `.packages` inconsistency | Open | [#12](https://github.com/garthtarr/mplot/issues/12) |
| P3 | Add parallelization to `bglmnet()` | Open | [#13](https://github.com/garthtarr/mplot/issues/13) |
| P3 | Fix `do.call(rbind, .)` data-processing bug in `bglmnet()` | Open | [#14](https://github.com/garthtarr/mplot/issues/14) |
| P3 | Reorder nested-loop parallelization in `af()` (flatten `n.c × B` grid) | Open | [#15](https://github.com/garthtarr/mplot/issues/15) |
| P4 | Progress bars, backend selection, benchmarking | Open | [#16](https://github.com/garthtarr/mplot/issues/16) |

**Latest update:** `af()` and `vis()` each called `parallel::stopCluster()` explicitly *and* registered it via `on.exit()`, closing the cluster twice and raising `invalid connection` errors — this was blocking the test suite. Fixed by removing the redundant explicit calls, relying on `on.exit()` alone. Verified with `cores = 1` and `cores = 2`. Tests updated to exercise the real code paths instead of skipping. See commit `8793a21`.

---

## Executive Summary

The mplot package uses parallel processing in three main functions (`af()`, `vis()`, and `bglmnet()`). Beyond the P1 cluster-closure bug (now fixed), the implementation has issues affecting:
- **Robustness** (global variable assignment inside workers, inconsistent package loading)
- **Memory efficiency** (large objects copied to every worker)
- **Modern R ecosystem alignment** (reliance on older foreach/doParallel/doRNG stack)
- **Completeness** (`bglmnet()` isn't parallelized at all, and has a separate, unrelated bug)

## Current Implementation Analysis

### 1. `af()` Function (R/af.R)

```r
cl.af <- makeCluster(cores)
on.exit(parallel::stopCluster(cl.af), add = TRUE)
doParallel::registerDoParallel(cl.af)

p.star.all <- foreach(
  j = 1:n.c,
  .combine = rbind,
  .packages = c("mplot"),
  .options.RNG = seed
) %dorng% {
  # Bootstrap loop over boundary values (n.c iterations)
  # Each iteration performs B bootstrap replications sequentially
  fence.mod <- list()
  for (i in 1:B) {
    # Model fitting and fence procedure
  }
  process.fn(fence.mod, fence.rank)
}
```

**Remaining issues:**
1. **Large data copying** — the entire `Xy` dataframe and `mfstar` model are copied to each worker.
2. **Nested loops** — parallelizes over `n.c` boundary values but keeps the `B` bootstrap replications sequential inside each worker (fewer, larger tasks — worse load balancing than the reverse).
3. **Global assignment in worker** — `initial.weights <<- m$wts` inside the `%dorng%` block is unnecessary and confusing; each worker already has its own copy of `m$wts` via lexical scoping.

### 2. `vis()` Function (R/vis.R)

```r
cl.visB <- parallel::makeCluster(cores)
on.exit(parallel::stopCluster(cl.visB), add = TRUE)
doParallel::registerDoParallel(cl.visB)

res <- foreach::foreach(
  b = 1:B,
  .packages = c("bestglm"),  # or "leaps" or "glmulti"
  .options.RNG = seed
) %dorng% {
  wts <- stats::rexp(n = n.obs, rate = 1) * initial.weights
  # Model selection on bootstrap sample
}
```

**Remaining issues:**
1. **Three parallel branches** — separate `%dorng%` blocks for glm+bestglm, glm+glmulti, and lm+leaps, each with its own `.packages` argument, increasing maintenance surface.
2. **Global assignment with `<<-`** in the glmulti branch:
   ```r
   mf <<- mf
   initial.weights <<- initial.weights
   n.obs <<- n.obs
   dryrun <<- dryrun
   ```
   Unnecessary and breaks encapsulation — these are already available via lexical scope.
3. **Package loading inconsistency** — `.packages` varies by branch and doesn't always include everything referenced inside the block.

### 3. `bglmnet()` Function (R/bglmnet.R)

```r
betaboot <- array(0, dim = c(kf, nlambda, B))
for (j in 1:B) {
  wts <- stats::rexp(n = n.obs, rate = 1) * m$wts
  for (i in 1:nlambda) {
    temp <- glmnet::glmnet(X, Y, alpha = 1, lambda = lambda[i],
                           family = fam, weights = wts)
    betaboot[, i, j] <- (temp$beta[, 1] != 0)
  }
}
```

**Remaining issues:**
1. **No parallelization at all** — fully sequential nested loops (`B` × `nlambda` model fits), despite being the most computationally intensive of the three functions.
2. **Separate data-processing bug** — lines ~156-158 raise `Error in do.call(rbind, x): second argument must be a list` for some inputs:
   ```r
   mod.sum <- betaboot |>
     apply(3, get_unique_mods) |>
     (\(x) do.call(rbind, x))() |>  # fails when apply() returns a matrix, not a list
     ...
   ```
   This is a correctness bug independent of parallelization and is why `test-bglmnet.R` still has a skipped test.

---

## Recommended Improvements

### P2: Migrate to `future` + `furrr`

Rationale: more explicit, better error propagation, no global-variable hassles, native reproducibility support via `furrr_options(seed = TRUE)`.

```r
library(future)
library(furrr)

old_plan <- future::plan()
on.exit(future::plan(old_plan), add = TRUE)
if (cores > 1) future::plan(future::multisession, workers = cores)

# af(): parallelize over c.range values
p.star.all <- furrr::future_map_dfr(
  seq_along(c.range),
  function(j) {
    fence.mod <- list()
    for (i in 1:B) {
      # ... existing per-bootstrap code ...
    }
    process.fn(fence.mod, fence.rank)
  },
  .options = furrr_options(seed = TRUE)
)
```

If avoiding new dependencies is preferred, `parallel::parLapply()` is a viable minimal alternative — see the appendix for a worked example.

### P3: Reorder nested-loop parallelization in `af()`

Currently parallelizes the outer loop (`n.c` boundary values, typically ~20) and keeps `B` (typically ~60) sequential inside. Flattening to parallelize over the full `n.c × B` grid gives more, smaller tasks and better load balancing:

```r
task_grid <- expand.grid(c_idx = seq_len(n.c), b_idx = seq_len(B))

results <- furrr::future_map_dfr(
  seq_len(nrow(task_grid)),
  function(idx) {
    j <- task_grid$c_idx[idx]
    ystar_col <- stats::simulate(object = mfstar, nsim = 1)
    # ... fence procedure for this single (c, bootstrap) pair ...
  },
  .options = furrr_options(seed = TRUE)
)

p.star.all <- results |> dplyr::group_by(c_idx) |> dplyr::summarise(...)
```

### P3: Add parallelization to `bglmnet()`

```r
betaboot_list <- furrr::future_map(
  1:B,
  function(b) {
    wts <- stats::rexp(n = n.obs, rate = 1) * m$wts
    beta_matrix <- matrix(0, nrow = kf, ncol = nlambda)
    for (i in 1:nlambda) {
      temp <- glmnet::glmnet(X, Y, alpha = 1, lambda = lambda[i],
                             family = fam, weights = wts)
      beta_matrix[, i] <- (temp$beta[, 1] != 0)
    }
    beta_matrix
  },
  .options = furrr_options(seed = TRUE)
)
betaboot <- array(unlist(betaboot_list), dim = c(kf, nlambda, B))
```

Note: the `do.call(rbind, .)` bug in the downstream summary step should be fixed independently of this — it will surface regardless of how `betaboot` is computed.

### P2: Fix global variable issues

Prefer one of:
1. Let `furrr`/`future` capture globals automatically (default behavior) instead of `<<-`.
2. If staying with `foreach`, pass values via `.export` rather than assigning into the parent frame from inside the worker.
3. Refactor the per-iteration body into a standalone function taking explicit parameters — clearer regardless of backend.

### P2: Fix package loading in parallel workers

```r
# Be exhaustive and explicit about what each branch needs:
.packages = c("mplot", "leaps", "bestglm")

# Or prefer explicit namespacing over relying on .packages/search path:
mplot::lmfence(...)
```

---

## Testing Improvements

Beyond the tests already added for the P1 fix, consider:

```r
test_that("vis produces identical results with same seed", {
  result1 <- vis(model, cores = 1, B = 5, seed = 999)
  result2 <- vis(model, cores = 1, B = 5, seed = 999)
  expect_identical(result1$res.df, result2$res.df)
})

test_that("bglmnet respects cores argument once parallelized", {
  result <- bglmnet(model, cores = 1, B = 5, nlambda = 10, seed = 123)
  expect_s3_class(result, "bglmnet")
})
```

---

## Migration Checklist

- [x] **Phase 1 (Immediate)** — commit `8793a21`
  - [x] Remove redundant `stopCluster(cl.af)` in af.R
  - [x] Remove redundant `parallel::stopCluster(cl.visB)` in vis.R
  - [x] Confirm tests pass with `devtools::test()`
  - [x] Enable previously-skipped `af()`/`vis()` tests

- [ ] **Phase 2 (Short-term)**
  - [ ] Add `future`, `furrr` to DESCRIPTION `Imports`
  - [ ] Refactor `af()` to use `furrr::future_map_dfr()`
  - [ ] Refactor `vis()` to use `furrr::future_map()`
  - [ ] Remove `<<-` global assignments in both functions
  - [ ] Update tests to verify seed-based reproducibility
  - [ ] Document cluster/backend behavior in function help

- [ ] **Phase 3 (Medium-term)**
  - [ ] Add parallelization to `bglmnet()`
  - [ ] Fix `do.call(rbind, .)` bug in `bglmnet()`'s model-summary step
  - [ ] Reorder `af()`'s nested-loop parallelization (flatten `n.c × B` grid)
  - [ ] Add comprehensive tests for `bglmnet()`

- [ ] **Phase 4 (Optional)**
  - [ ] Add progress bars via `progressr`
  - [ ] Add parallel backend selection option
  - [ ] Add benchmarking / automatic core-count heuristics

---

## Performance Considerations

- **Overhead vs. benefit**: cluster creation and data copying have fixed overhead; for small problems (n < 200, p < 50) sequential (`cores = 1`) may outperform parallel.
- **Memory**: each worker holds its own copy of large objects (`X`, `Xy`, fitted models); with 8 workers, expect roughly 8x memory footprint for large datasets.
- **Optimization tips**: avoid sending unnecessary objects to workers, parallelize over larger/more numerous tasks rather than small inner loops, profile with `bench::mark()` before and after changes, and keep `cores = 1` as a safe interactive default.

---

## References

- [future Package Documentation](https://cran.r-project.org/web/packages/future/)
- [furrr Package for Parallelization](https://cran.r-project.org/web/packages/furrr/)
- [doRNG and Reproducibility](https://cran.r-project.org/web/packages/doRNG/)
- [High Performance Computing Task View](https://cran.r-project.org/view/HighPerformanceComputing)

test_that("zero-count markets are included in every subsector", {
  out <- run_density(pop_table = toy_pop())
  y20 <- out[out$YEAR == 2020 & out$GEO != "ALL", ]

  # 4 markets x (ART, EDU, ALL) in 2020
  expect_equal(nrow(y20), 12)
  expect_equal(cell(out, "D", "ART")$N, 0)
  expect_equal(cell(out, "B", "EDU")$N, 0)
  expect_equal(cell(out, "A", "ART")$N, 4)          # counts the NA-revenue org
  expect_equal(cell(out, "A", "ART")$TOTREV, 100)
})

test_that("national rows are not fitted and carry national population", {
  out <- run_density(pop_table = toy_pop())
  nat <- cell(out, "ALL", "ART")
  expect_equal(nat$POP, 1e5 + 5e4 + 2e4 + 1e4)
  expect_equal(nat$N, 6)                            # incl. NA-geo and NA-revenue orgs
  expect_true(is.na(nat$DENSITY_PCTL_N))

  # The ART 2020 model uses the four markets only
  m <- cell(out, "A", "ART")$model_n[[1]]
  expect_equal(stats::nobs(m), 4)
})

test_that("loglinear densities match a direct regression", {
  out <- run_density(pop_table = toy_pop(), model = "loglinear")
  art <- out[out$SUB == "ART" & out$YEAR == 2020 & out$GEO != "ALL", ]
  n   <- c(A = 4, B = 1, C = 0, D = 0)[art$GEO]
  pop <- c(A = 1e5, B = 5e4, C = 2e4, D = 1e4)[art$GEO]
  m <- stats::lm(log(n + 1) ~ log(pop + 1))
  expected <- pmin(exp(stats::residuals(m)) - 1, 2)
  expect_equal(art$DENSITY_PCTL_N, unname(expected))
  expect_equal(attr(out, "density_model"), "loglinear")
})

test_that("Poisson densities are observed / expected - 1 from a direct GLM", {
  out <- run_density(pop_table = toy_pop(), model = "poisson")
  art <- out[out$SUB == "ART" & out$YEAR == 2020 & out$GEO != "ALL", ]
  n   <- c(A = 4, B = 1, C = 0, D = 0)[art$GEO]
  pop <- c(A = 1e5, B = 5e4, C = 2e4, D = 1e4)[art$GEO]
  m <- stats::glm(n ~ log(pop + 1), family = stats::poisson())
  expect_equal(art$EXPECTED_N, unname(stats::fitted(m)))
  expect_equal(art$DENSITY_PCTL_N, pmin(unname(n / stats::fitted(m) - 1), 2))
  expect_equal(cell(out, "D", "ART")$DENSITY_PCTL_N, -1)   # zero cell
})

test_that("resource density uses a quasi-Poisson GLM for count models", {
  out <- run_density(pop_table = toy_pop(), model = "poisson")
  m <- cell(out, "A", "ART")$model_r[[1]]
  expect_s3_class(m, "glm")
  expect_equal(stats::family(m)$family, "quasipoisson")
  a <- cell(out, "A", "ART")
  expect_equal(a$DENSITY_PCTL_R, min(a$TOTREV / a$EXPECTED_TOTREV - 1, 2))
})

test_that("negative binomial is the default and falls back cleanly", {
  out <- run_density(pop_table = toy_pop())
  expect_equal(attr(out, "density_model"), "negbin")
  m <- cell(out, "A", "ART")$model_n[[1]]
  expect_s3_class(m, "glm")                         # negbin or Poisson fallback
  expect_true(all(is.finite(out$EXPECTED_N[out$GEO != "ALL" & out$YEAR == 2020])))
})

test_that("without pop_table, markets come from org rows", {
  out <- run_density()
  expect_false("D" %in% out$GEO)
  expect_equal(cell(out, "C", "ART")$N, 0)          # C has orgs, none in ART
})

test_that("too few markets give NA densities instead of an error", {
  out <- run_density()
  expect_true(all(is.na(out$DENSITY_PCTL_N[out$YEAR == 2021])))   # 1 market
})

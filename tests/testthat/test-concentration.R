test_that("HHI and NHHI match hand calculations", {
  out <- run_toy(get_nhhi)

  expect_equal(cell(out, "A", "ART")$HHI, 0.46)
  expect_equal(cell(out, "A", "ART")$NHHI, (0.46 - 1/3) / (1 - 1/3))
  expect_equal(cell(out, "A", "ART")$N, 3)          # NA revenue dropped
  expect_equal(cell(out, "A", "ART")$TOTREV, 100)
  expect_equal(cell(out, "A", "EDU")$HHI, 0.5)
  expect_equal(cell(out, "A", "EDU")$NHHI, 0)
  expect_equal(cell(out, "A", "ALL")$HHI, 0.24)
  expect_equal(cell(out, "B", "ART")$HHI, 1)
  expect_equal(cell(out, "B", "ART")$NHHI, 0)       # single-org convention
  expect_true(is.na(cell(out, "C", "EDU")$HHI))     # all-zero market
  expect_equal(cell(out, "A", "ART", 2021)$HHI, 0.68)
})

test_that("get_hhi has the same values and rollups as get_nhhi", {
  h <- run_toy(get_hhi)
  n <- run_toy(get_nhhi)
  expect_equal(h$HHI, n$HHI)
  expect_false("NHHI" %in% names(h))
  expect_true("ALL" %in% h$GEO)
})

test_that("rollups handle missing geography correctly", {
  out <- run_toy(get_nhhi)
  expect_false(any(is.na(out$GEO)))                  # no NA market rows
  expect_equal(cell(out, "ALL", "ART")$N, 5)         # includes NA-geo org
  expect_equal(cell(out, "ALL", "ART")$TOTREV, 225)
  expect_equal(cell(out, "A", "ALL")$N, 5)
})

test_that("concentration ratios match hand calculations", {
  out <- run_toy(get_cr_all, numbers = 1:3)

  expect_equal(cell(out, "A", "ART")$CR1, 0.6)
  expect_equal(cell(out, "A", "ART")$CR2, 0.9)
  expect_equal(cell(out, "A", "ART")$CR3, 1)        # n <= k
  expect_equal(cell(out, "A", "ALL")$CR1, 0.3)
  expect_equal(cell(out, "A", "ALL")$CR2, 0.55)
  expect_equal(cell(out, "A", "ALL")$CR3, 0.8)
  expect_true(is.na(cell(out, "C", "EDU")$CR1))     # all-zero market

  pct <- run_toy(get_cr_all, numbers = 1, as_percent = TRUE)
  expect_equal(cell(pct, "A", "ART")$CR1, 60)

  single <- run_toy(get_cr, number = 2)
  expect_equal(cell(single, "A", "ART")$CR2, 0.9)
})

test_that("Kwoka index matches hand calculations", {
  out <- run_toy(get_kwoka)
  expect_equal(cell(out, "A", "ART")$KWOKA, 0.46)
  expect_equal(cell(out, "A", "ART")$top1, 0.6)
  expect_equal(cell(out, "A", "EDU")$KWOKA, 1)      # fewer than 3 orgs
  # A / ALL: top three shares .30, .25, .25 of total 200
  expect_equal(cell(out, "A", "ALL")$KWOKA, sum((c(.3, .25, .25) / .8)^2))
})

test_that("Gini matches hand calculations", {
  out <- run_toy(get_gini)
  expect_equal(cell(out, "A", "ART")$GINI, 0.5)     # unbiased Gini of 60/30/10
  expect_equal(cell(out, "A", "EDU")$GINI, 0)
  expect_equal(cell(out, "B", "ART")$GINI, 1)       # single-org convention
  expect_true(is.na(cell(out, "C", "EDU")$GINI))
})

test_that("missing subsectors become UNK", {
  d <- toy_panel()
  d$SUB[1] <- NA
  out <- get_hhi(d, "GEO", "SUB", "TOTREV", "YEAR")
  expect_true("UNK" %in% out$SUB)
})

test_that("NTEEV2 codes are cut to the major group", {
  d <- data.frame(G = "A", NTEEV2 = c("ART-A20-RG", "ART-A50-RG", "EDU-B20-RG"),
                  TOTREV = c(1, 1, 1), Y = 2020)
  out <- get_hhi(d, "G", "NTEEV2", "TOTREV", "Y")
  expect_setequal(out$SUB[out$GEO == "A"], c("ART", "EDU", "ALL"))
})

test_that("missing columns give an informative error", {
  expect_error(get_hhi(toy_panel(), geography = "NOPE"), "not found")
})

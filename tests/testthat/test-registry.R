# One market with hand-checkable composition and age values.
comp_panel <- function() {
  data.frame(GEO = "A", SUB = "ART", YEAR = 2020,
             TOTREV      = c(100, 100, 200, 0),
             REVPROG     = c(80, 20, 50, 0),
             REVCONT     = c(10, 70, 150, 0),
             FNDFEE      = c(0, 10, NA, 0),
             RULING_YEAR = c(2000, 2010, 2015, 2021))
}

test_that("density per capita counts every org and zero-fills markets", {
  d <- get_density_per_capita(toy_panel(), "GEO", "SUB", "YEAR",
                              pop_table = toy_pop(), resource = "TOTREV",
                              small_max = 50, large_min = 60)
  a <- cell(d, "A", "ART")
  expect_equal(a$N, 4)                               # includes the NA-revenue org
  expect_equal(a$DENSITY_PER_100K, 4)
  expect_equal(a$N_RES, 3)
  expect_equal(a$N_SMALL, 2)                         # 30, 10
  expect_equal(a$N_LARGE, 1)                         # 60
  expect_equal(a$SMALL_SHARE, 2 / 3)
  expect_equal(cell(d, "D", "ART")$N, 0)
  expect_equal(cell(d, "D", "ART")$DENSITY_PER_100K, 0)
  expect_true(is.na(cell(d, "D", "ART")$SMALL_SHARE))
  nat <- cell(d, "ALL", "ART")
  expect_equal(nat$N, 6)
  expect_equal(nat$POP, 1.8e5)
})

test_that("density per capita works without a resource", {
  d <- get_density_per_capita(toy_panel(), "GEO", "SUB", "YEAR", pop_table = toy_pop())
  expect_false("N_SMALL" %in% names(d))
  expect_equal(cell(d, "A", "ALL")$N, 6)
})

test_that("median age matches hand calculation", {
  a <- get_median_age(comp_panel(), "GEO", "SUB", "YEAR")
  expect_equal(cell(a, "A", "ART")$MEDIAN_AGE, 10)  # 20, 10, 5; 2021 ruling dropped
  expect_equal(cell(a, "A", "ART")$N_AGE, 3)
})

test_that("commercial share matches hand calculation", {
  cs <- get_commercial_share(comp_panel(), "GEO", "SUB", "YEAR")
  a <- cell(cs, "A", "ART")
  expect_equal(a$N_COMM, 3)                          # zero-revenue org excluded
  expect_equal(a$COMMERCIAL_SHARE, 1 / 3)            # only 80/100 is above half
  expect_equal(a$PROGRAM_REVENUE_SHARE, 150 / 400)
})

test_that("fundraising ratio matches hand calculation", {
  fr <- get_fundraising_ratio(comp_panel(), "GEO", "SUB", "YEAR")
  a <- cell(fr, "A", "ART")
  expect_equal(a$N_FEE, 3)                           # NA fee excluded
  expect_equal(a$FUNDRAISER_SHARE, 1 / 3)
  expect_equal(a$FUNDRAISING_RATIO, 7)               # 70 / 10
})

test_that("the registry lists metrics and universes", {
  reg <- npc_metrics()
  expect_true(all(c("HHI", "DENSITY_PER_100K", "MEDIAN_AGE", "CR8") %in% reg$id))
  expect_false(anyDuplicated(reg$id) > 0)
  g <- npc_metrics("grassroots")
  expect_false(any(g$financial))
  expect_true(all(c("DENSITY_PER_100K", "DENSITY_PCTL_N", "MEDIAN_AGE") %in% g$id))
})

test_that("financial metrics are refused for non-filer universes", {
  expect_error(resolve_metrics("HHI", "grassroots"),
               "HHI requires financial data; grassroots organizations")
  expect_error(resolve_metrics(c("HHI", "GINI"), "registered"), "require financial data")
  expect_error(resolve_metrics("NOPE"), "Unknown metric")
  expect_equal(resolve_metrics("default", "grassroots"), npc_metrics("grassroots")$id)
})

test_that("npc_compute_metrics runs only the requested families", {
  m <- npc_compute_metrics(toy_panel(), geography = "GEO", subsector = "SUB",
                           year = "YEAR", pop_table = toy_pop(),
                           metrics = c("DENSITY_PER_100K", "HHI", "CR2"))
  expect_named(m, c("GEO", "SUB", "YEAR", "N", "POP", "N_RES", "TOTREV",
                    "DENSITY_PER_100K", "HHI", "CR2"))
  a <- cell(m, "A", "ART")
  expect_equal(a$HHI, 0.46)
  expect_equal(a$CR2, 0.9)
  expect_equal(a$N, 4)
  expect_equal(a$N_RES, 3)
  expect_true(is.na(cell(m, "D", "ART")$HHI))        # zero cell
  expect_equal(cell(m, "D", "ART")$N, 0)
  expect_equal(attr(m, "metrics"), c("DENSITY_PER_100K", "HHI", "CR2"))
})

test_that("count-only requests skip the resource", {
  m <- npc_compute_metrics(toy_panel(), geography = "GEO", subsector = "SUB",
                           year = "YEAR", pop_table = toy_pop(),
                           metrics = "DENSITY_PER_100K", universe = "grassroots")
  expect_false(any(c("N_RES", "TOTREV") %in% names(m)))
})

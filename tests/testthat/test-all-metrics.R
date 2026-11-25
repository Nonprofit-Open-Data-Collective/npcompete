test_that("check_year_completeness flags partial years", {
  yrs <- c(rep(2019, 100), rep(2020, 98), rep(2021, 101), rep(2022, 40))
  yc <- check_year_completeness(yrs)
  expect_equal(yc$YEAR[yc$INCOMPLETE], 2022)
})

test_that("get_all_metrics returns one row per GEO-SUB-YEAR", {
  out <- suppressMessages(
    get_all_metrics(toy_panel(), geography = "GEO", population = "POP",
                    subsector = "SUB", resource = "TOTREV", year = "YEAR",
                    cr_n = 1:3, pop_table = toy_pop(),
                    drop_incomplete_years = FALSE))
  keys <- paste(out$GEO, out$SUB, out$YEAR)
  expect_false(anyDuplicated(keys) > 0)
  expect_equal(cell(out, "A", "ART")$HHI, 0.46)
  expect_equal(cell(out, "D", "ART")$N, 0)
  expect_true(all(c("GINI", "NHHI", "KWOKA", "CR3", "DENSITY_PCTL_N") %in% names(out)))
})

test_that("get_all_metrics drops incomplete years by default", {
  expect_message(
    out <- get_all_metrics(toy_panel(), geography = "GEO", population = "POP",
                           subsector = "SUB", resource = "TOTREV",
                           year = "YEAR", cr_n = 1),
    "Dropping incomplete years: 2021")
  expect_false(2021 %in% out$YEAR)
})

test_that("legacy functions warn that they are deprecated", {
  expect_warning(try(get_density(data.frame(), "a", "b", "c", 2010), silent = TRUE),
                 "deprecated")
})

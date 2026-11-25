# Dashboard data handling and app construction.

test_that("dashboard data defaults to the bundled CBSA metrics", {
  d <- npcompete:::dashboard_data()
  expect_true(all(c("GEO_LEVEL", "GEO", "SUB", "YEAR", "UNIVERSE", "RESOURCE",
                    "METRIC", "VALUE", "NAME") %in% names(d)))
  expect_setequal(unique(d$GEO_LEVEL), "cbsa")
  expect_true("HHI" %in% d$METRIC)
  expect_false(any(grepl("^EXPECTED_", d$METRIC)))
  expect_equal(unique(d$NAME[d$GEO == "35620"]),
               unique(pop_cbsa$CBSA_NAME[pop_cbsa$CBSA_FIPS == "35620"]))
})

test_that("dashboard data accepts long, wide and written output", {
  arch <- build_tmp_archive()
  long <- suppressMessages(npc_build_metrics(2019:2020, geo = c("cbsa", "county"),
                                             resources = "TOTREV", dir = arch))
  d <- npcompete:::dashboard_data(long)
  expect_setequal(unique(d$GEO_LEVEL), c("cbsa", "county"))
  expect_equal(unique(d$NAME[d$GEO == "50007"]), "Chittenden, VT")
  expect_equal(unique(d$NAME[d$GEO == "ALL"]), "United States")

  both <- npcompete:::dashboard_data(list(long, transform(long, UNIVERSE = "registered")))
  expect_setequal(unique(both$UNIVERSE), c("filers", "registered"))

  wide <- suppressMessages(npc_build_metrics(2020, geo = "county", resources = "TOTREV",
                                             dir = arch, format = "wide"))
  dw <- npcompete:::dashboard_data(wide)
  expect_true(all(c("HHI", "RESOURCE_TOTAL") %in% dw$METRIC))
  expect_setequal(unique(dw$RESOURCE[dw$METRIC == "HHI"]), "TOTREV")

  out <- file.path(withr_tempdir(), "m")
  suppressMessages(npc_build_metrics(2020, geo = "county", resources = "TOTREV",
                                     dir = arch, format = "wide", out_dir = out))
  dc <- npcompete:::dashboard_data(out)
  expect_equal(sort(unique(dc$METRIC)), sort(unique(dw$METRIC)))
  expect_setequal(unique(dc$RESOURCE[dc$METRIC == "HHI"]), "TOTREV")

  expect_error(npcompete:::dashboard_data(42), "must be output")
})

test_that("npc_dashboard builds a Shiny app whose outputs render", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- npc_dashboard(launch = FALSE)
  expect_s3_class(app, "shiny.appobj")

  d <- npcompete:::dashboard_data()
  shiny::testServer(npcompete:::dashboard_server(d), {
    session$setInputs(universe = "filers", level = "cbsa", metric = "HHI",
                      resource = "TOTREV", sub = "ALL", year = "2019",
                      market = "35620", metric2 = "N")
    v <- selected()
    expect_true(nrow(v) > 900)
    expect_true("35620" %in% v$GEO)
    expect_match(paste(unlist(output$dist_note), collapse = " "), "higher than")
    expect_true(nrow(table_rows()) == nrow(v))
  })
})

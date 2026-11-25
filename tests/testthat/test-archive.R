test_that("npc_standardize maps the SOI panel to the canonical schema", {
  s <- npc_standardize(soi_raw())
  expect_named(s, npcompete:::canonical_cols)
  expect_equal(s$COUNTY_FIPS[1], formatC(soi_raw()$COUNTY_FIPS[1], width = 5, flag = "0"))
  expect_setequal(stats::na.omit(unique(s$FILER_TYPE)), c("990", "990EZ"))
  expect_true(s$IMPUTED[5])
  expect_true(is.na(s$FILER_TYPE[5]))
  expect_true(s$REVOKED[9])
  expect_equal(s$REVOCATION_YEAR[9], 2020L)
  expect_type(s$YEAR, "integer")
  expect_error(npc_standardize(data.frame(x = 1)), "missing columns")
})

test_that("duplicate EIN-years keep the filing NCCS keeps", {
  raw <- soi_raw()
  base <- raw[raw$EIN2 == "EIN-00-0000001" & raw$TAXYEAR == 2019, ]
  short <- base; short$SOURCE <- "SOI_2021_PC"; short$FISMO <- 6; short$TOTREV <- 111
  early <- base; early$SOURCE <- "SOI_2021_PC"; early$FISMO <- 3; early$TOTREV <- 222
  imp <- base; imp$SOURCE <- "SOI_2022_PC"; imp$MISSING_YEAR <- 1; imp$TOTREV <- NA
  s <- npc_standardize(rbind(raw, short, early, imp, base))
  expect_equal(anyDuplicated(s[c("EIN2", "YEAR")]), 0L)
  expect_equal(nrow(s), nrow(unique(raw[c("EIN2", "TAXYEAR")])))
  # latest source file, then latest period end; imputed rows lose
  expect_equal(s$TOTREV[s$EIN2 == "EIN-00-0000001" & s$YEAR == 2019], 111)

  # an archive written with duplicates is deduplicated when read
  skip_if_not_installed("arrow")
  arch <- withr_tempdir()
  d <- rbind(s, transform(s[1, ], SOURCE = "SOI_2019_PC", TOTREV = -1))
  arrow::write_dataset(d, file.path(arch, "filers"), partitioning = "YEAR")
  utils::write.csv(data.frame(key = "source", value = "soi_panel"),
                   file.path(arch, "manifest.csv"), row.names = FALSE)
  utils::write.csv(data.frame(YEAR = 2018:2021, N = c(20, 20, 20, 2)),
                   file.path(arch, "year-counts.csv"), row.names = FALSE)
  o <- npc_get_orgs(s$YEAR[1], dir = arch, subsection = NULL)
  expect_equal(anyDuplicated(o[c("EIN2", "YEAR")]), 0L)
  expect_false(-1 %in% o$TOTREV)
})

test_that("archive round trip returns the requested years only", {
  arch <- build_tmp_archive()
  info <- npc_archive_info(arch)
  expect_equal(info$year_counts$YEAR, 2018:2021)
  expect_equal(info$year_counts$INCOMPLETE, c(FALSE, FALSE, FALSE, TRUE))

  o <- npc_get_orgs(2019:2020, dir = arch)
  expect_setequal(unique(o$YEAR), 2019:2020)
  expect_true(all(o$SUBSECTION == 3))
  expect_true("CBSA_FIPS" %in% names(o))

  all_subs <- npc_get_orgs(2019, dir = arch, subsection = NULL)
  expect_true(4L %in% all_subs$SUBSECTION)

  no_imp <- npc_get_orgs(2018:2020, dir = arch, include_imputed = FALSE)
  expect_false(any(no_imp$IMPUTED))

  expect_error(npc_get_orgs(2010, dir = arch), "not in the archive")
  expect_error(npc_build_archive(file.path(arch, "x.csv"), dir = arch), "already exists")
})

test_that("CBSA comes from the crosswalk or the geocode", {
  arch <- build_tmp_archive()
  xw <- npc_get_orgs(2019, dir = arch)
  gc <- npc_get_orgs(2019, dir = arch, cbsa = "geocode")
  expect_equal(xw$CBSA_FIPS,
               cbsa_county_xwalk$CBSA_FIPS[match(xw$COUNTY_FIPS, cbsa_county_xwalk$COUNTY_FIPS)])
  expect_equal(gc$CBSA_FIPS, gc$CBSA_FIPS_GEOCODE)
})

test_that("planned universes and sources give clear errors", {
  expect_error(npc_get_orgs(2019, universe = "registered", dir = "."), "Phase 5")
  expect_error(npc_get_orgs(2019, source = "nccs", cbsa = "geocode", dir = "."),
               "no geocoded CBSA")
})

test_that("npc_get_population returns GEO / YEAR / POP", {
  p <- npc_get_population("cbsa", 2020)
  expect_named(p, c("GEO", "YEAR", "POP"))
  expect_true(all(p$YEAR == 2020))
  # CBSA populations equal the sum of their counties
  ny <- cbsa_county_xwalk$COUNTY_FIPS[cbsa_county_xwalk$CBSA_FIPS == "35620"]
  cty <- sum(pop_county$POP[pop_county$COUNTY_FIPS %in% ny & pop_county$YEAR == 2020])
  expect_equal(p$POP[p$GEO == "35620"], cty)
})

test_that("npc_build_metrics builds long and wide output", {
  arch <- build_tmp_archive()
  expect_message(
    long <- npc_build_metrics(2018:2021, geo = c("cbsa", "county"),
                              resources = c("TOTREV", "NETASS"), dir = arch),
    "with incomplete data: 2021")
  expect_named(long, c("GEO_LEVEL", "GEO", "SUB", "YEAR", "UNIVERSE",
                       "RESOURCE", "METRIC", "VALUE"))
  expect_setequal(unique(long$YEAR), 2018:2020)
  expect_setequal(unique(long$GEO_LEVEL), c("cbsa", "county"))
  expect_true(all(c("HHI", "DENSITY_PCTL_N", "RESOURCE_TOTAL") %in% long$METRIC))
  expect_s3_class(attr(long, "manifest"), "data.frame")

  wide <- suppressMessages(
    npc_build_metrics(2019:2020, geo = "county", resources = "TOTREV",
                      dir = arch, format = "wide"))
  expect_named(wide, "county_TOTREV")
  w <- wide$county_TOTREV
  # every county in Vermont appears, including those with no orgs
  vt <- unique(pop_county$COUNTY_FIPS[pop_county$STATE_FIPS == "50"])
  expect_true(all(vt %in% w$GEO))
  nat <- w[w$GEO == "ALL" & w$SUB == "ALL" & w$YEAR == 2020, ]
  expect_equal(nat$POP, sum(pop_county$POP[pop_county$YEAR == 2020]))
})

test_that("npc_build_metrics writes CSVs and a manifest", {
  arch <- build_tmp_archive()
  out <- file.path(withr_tempdir(), "metrics")
  suppressMessages(
    npc_build_metrics(2019:2020, geo = "cbsa", resources = "TOTREV",
                      dir = arch, format = "wide", out_dir = out))
  expect_true(file.exists(file.path(out, "NPCOMPETE-METRICS-BY-CBSA-TOTREV.CSV")))
  mf <- utils::read.csv(file.path(out, "manifest.csv"))
  expect_equal(mf$value[mf$key == "years"], "2019,2020")
})

test_that("npc_build_metrics long format keeps one copy of resource-free metrics", {
  arch <- build_tmp_archive()
  long <- suppressMessages(
    npc_build_metrics(2020, geo = "county", resources = c("TOTREV", "NETASS"),
                      dir = arch))
  dens <- long[long$METRIC == "DENSITY_PER_100K", ]
  expect_true(all(is.na(dens$RESOURCE)))
  expect_false(anyDuplicated(paste(dens$GEO, dens$SUB, dens$YEAR)) > 0)
  expect_setequal(unique(long$RESOURCE[long$METRIC == "HHI"]), c("TOTREV", "NETASS"))
})

test_that("count-only builds give one table per geography", {
  arch <- build_tmp_archive()
  wide <- suppressMessages(
    npc_build_metrics(2020, geo = "county", metrics = c("DENSITY_PER_100K", "MEDIAN_AGE"),
                      dir = arch, format = "wide"))
  expect_named(wide, "county")
  expect_true(all(c("DENSITY_PER_100K", "MEDIAN_AGE") %in% names(wide$county)))
  expect_error(npc_build_metrics(2020, metrics = "HHI", universe = "grassroots", dir = arch),
               "requires financial data")
})

test_that("rolling windows pool years and drop years without a full window", {
  arch <- build_tmp_archive()
  expect_message(
    w <- npc_build_metrics(2018:2020, geo = "county", window = 2,
                           metrics = c("DENSITY_PER_100K", "HHI"),
                           resources = "TOTREV", dir = arch, format = "wide"),
    "missing window years: 2018")
  t <- w$county_TOTREV
  expect_setequal(unique(t$YEAR), 2019:2020)
  # counts are organizations active in 2019-2020 (no exits in the fixture)
  raw <- npc_get_orgs(2019:2020, dir = arch)
  expect_equal(t$N[t$GEO == "ALL" & t$SUB == "ALL" & t$YEAR == 2020],
               length(unique(raw$EIN2)))
  # population is the two-year average
  p <- npc_get_population("county", 2019:2020)
  expect_equal(t$POP[t$GEO == "50007" & t$SUB == "ALL" & t$YEAR == 2020],
               mean(p$POP[p$GEO == "50007"]))
  expect_error(npc_build_metrics(2020, window = 0, dir = arch), "whole number")
})

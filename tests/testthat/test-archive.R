# A tiny SOI-panel-layout table: Vermont counties, 2018-2020, with an
# imputed gap row, a revocation, a 501(c)(4) and a partial year (2021).
soi_raw <- function() {
  set.seed(1)
  counties <- c("50007", "50021", "50023", "50025", "50027")  # 50007/50021/50023 in CBSAs
  n <- 60
  d <- data.frame(
    EIN2 = sprintf("EIN-00-%07d", rep(1:20, each = 3)),
    TAXYEAR = rep(2018:2020, times = 20),
    COUNTY_FIPS = as.integer(rep(sample(counties, 20, replace = TRUE), each = 3)),
    CENSUS_CBSA_FIPS = NA,
    NTEEV2 = rep(sample(c("ART-A20-RG", "HMS-P20-RG", "EDU-B20-RG"), 20, TRUE), each = 3),
    V501C = 3,
    SOURCE = sample(c("SOI_2020_PC", "SOI_2020_EZ"), n, TRUE),
    EFILE_X = "EFILE",
    ORG_RULING_YEAR = 1995,
    REVOKED_YEAR_X = 0,
    revocation_date = "",
    exemption_reinstatement_date = "",
    TOTREV = round(stats::rlnorm(n, 11, 1.5)),
    TOTEXP = round(stats::rlnorm(n, 11, 1.5)),
    NETASS = round(stats::rnorm(n, 1e5, 5e4)),
    TOTASS = 2e5, TOTLIA = 1e5, REVPROG = 10, REVCONT = 100, REVINVE = 1,
    FNDFEE = NA, FISYEAR = rep(2018:2020, times = 20), FISMO = 12,
    MISSING_YEAR = 0,
    stringsAsFactors = FALSE
  )
  d[5, c("SOURCE", "TOTREV", "TOTEXP", "NETASS", "MISSING_YEAR")] <- list("", NA, NA, NA, 1)
  d[9, c("REVOKED_YEAR_X", "revocation_date")] <- list(1, "15-MAY-2020")
  d$V501C[58:60] <- 4
  d$CENSUS_CBSA_FIPS[d$COUNTY_FIPS == 50007] <- 15540
  extra <- d[1:2, ]; extra$TAXYEAR <- 2021; extra$FISYEAR <- 2021
  rbind(d, extra)
}

build_tmp_archive <- function() {
  skip_if_not_installed("arrow")
  skip_if_not_installed("data.table")
  tmp <- withr_tempdir()
  csv <- file.path(tmp, "soi.csv")
  data.table::fwrite(soi_raw(), csv)
  arch <- file.path(tmp, "archive")
  suppressMessages(npc_build_archive(csv, dir = arch))
  arch
}

withr_tempdir <- function() {
  d <- tempfile("npc-")
  dir.create(d)
  d
}


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
  expect_error(npc_get_orgs(2019, source = "nccs", dir = "."), "Phase 4")
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
    "Dropping incomplete years: 2021")
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
  expect_true(file.exists(file.path(out, "CONCENTRATION-METRICS-BY-CBSA-TOTREV.CSV")))
  mf <- utils::read.csv(file.path(out, "manifest.csv"))
  expect_equal(mf$value[mf$key == "years"], "2019,2020")
})

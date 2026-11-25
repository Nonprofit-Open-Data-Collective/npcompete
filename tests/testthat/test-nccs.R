# Live NCCS mode, tested offline against a fake bucket in a local folder.

fake_bmf <- function() {
  data.frame(
    EIN2 = sprintf("EIN-00-%07d", 1:6),
    geo_county = c("Chittenden County", "Washington County", "City of Baltimore",
                   "Baltimore County", NA, "Doña Ana County"),
    geo_state_abbr = c("VT", "VT", "MD", "MD", NA, "NM"),
    nteev2 = c("ART-A20-RG", "HMS-P20-RG", "EDU-B20-RG", "ART-A20-RG",
               "HMS-P20-RG", "ENV-C20-RG"),
    subsection_code = "03",
    ruling_date = c("1990-05-01", "2001-01-01", "1900-01-01", "2015-03-01",
                    "1980-01-01", "2010-07-01"),
    bmf_vintage_ym = "2026-08",
    stringsAsFactors = FALSE
  )
}

# CORE rows for one fiscal-year-end file year. EIN 1 has a June year end,
# EIN 2 files twice (an amended return in a later extract).
fake_core <- function(fy, n_extra = 0) {
  d <- data.frame(
    ein = sprintf("00-%07d", c(1, 2, 2, 3, 4, 5, 6)),
    tax_year = fy,
    tax_month = c(6, 12, 12, 12, 12, 12, 12),
    tax_period = paste0(fy, c("06", "12", "12", "12", "12", "12", "12")),
    subsection_cd = c(3, 3, 3, 3, 3, 3, 4),
    source_form = c("990", "990", "990", "990ez", "990", "990ez", "990"),
    efile_indicator = TRUE,
    extract_year = c(fy, fy, fy + 1, fy, fy, fy, fy),
    total_revenue = c(100, 200, 250, 50, 1e6, 10, 5) + fy,
    total_expenses = 90, total_net_assets_eoy = 30, total_assets_eoy = 60,
    total_liabilities_eoy = 30, program_service_revenue = 5,
    total_contributions = 40, investment_income = 1,
    is_501c3 = TRUE,
    stringsAsFactors = FALSE
  )
  if (n_extra > 0) {
    x <- d[rep(4, n_extra), ]
    x$ein <- sprintf("99-%07d", seq_len(n_extra))
    d <- rbind(d, x)
  }
  d
}

fake_bucket <- function(file_years = 2018:2021, partial = 2021) {
  skip_if_not_installed("arrow")
  root <- withr_tempdir()
  b <- file.path(root, "bucket")
  p <- file.path(b, dirname(npcompete:::nccs_bmf_key))
  dir.create(p, recursive = TRUE)
  arrow::write_parquet(fake_bmf(), file.path(b, npcompete:::nccs_bmf_key))
  for (fy in file_years) {
    key <- npcompete:::nccs_core_key(fy)
    dir.create(file.path(b, dirname(key)), recursive = TRUE)
    core <- if (fy %in% partial) fake_core(fy)[c(1, 4), ] else fake_core(fy, n_extra = 3)
    arrow::write_parquet(core, file.path(b, key))
  }
  url <- paste0("file:///", sub("^/", "", normalizePath(b, winslash = "/")))
  list(url = url, cache = file.path(root, "cache"))
}


test_that("npc_match_county handles county types and independent cities", {
  got <- npc_match_county(
    c("Cook County", "City of Baltimore", "Baltimore County", "Baltimore city",
      "Doña Ana County", "Lafourche Parish", "Municipality of Anchorage",
      "St. Louis city", "St Louis County", "Nowhere County", NA, "District of Columbia"),
    c("IL", "MD", "MD", "MD", "NM", "LA", "AK", "MO", "MO", "VT", "VT", "DC"))
  expect_equal(got, c("17031", "24510", "24005", "24510", "35013", "22057",
                      "02020", "29510", "29189", NA, NA, "11001"))
})

test_that("npc_standardize maps CORE 990combined rows to the canonical schema", {
  s <- npc_standardize(fake_core(2019), source = "nccs_core", bmf = fake_bmf())
  expect_named(s, npcompete:::canonical_cols)
  # one row per EIN and tax year; the later extract wins
  expect_equal(anyDuplicated(s[c("EIN2", "YEAR")]), 0L)
  expect_equal(s$TOTREV[s$EIN2 == "EIN-00-0000002"], 250 + 2019)
  # a June 2019 year end is tax year 2018
  expect_equal(s$YEAR[s$EIN2 == "EIN-00-0000001"], 2018L)
  expect_equal(s$FISYEAR[s$EIN2 == "EIN-00-0000001"], 2019L)
  expect_equal(s$COUNTY_FIPS[match(sprintf("EIN-00-%07d", 1:4), s$EIN2)],
               c("50007", "50023", "24510", "24005"))
  expect_true(is.na(s$COUNTY_FIPS[s$EIN2 == "EIN-00-0000005"]))
  expect_equal(s$FILER_TYPE[s$EIN2 == "EIN-00-0000003"], "990EZ")
  expect_true(is.na(s$RULING_YEAR[s$EIN2 == "EIN-00-0000003"]))   # 1900 placeholder
  expect_equal(s$RULING_YEAR[s$EIN2 == "EIN-00-0000004"], 2015L)
  expect_true(all(is.na(s$FNDFEE)))
  # 990-EZ net assets come from the balance sheet; 990 keeps the reported line
  core <- fake_core(2019)
  core$total_net_assets_eoy <- 999
  s2 <- npc_standardize(core, source = "nccs_core", bmf = fake_bmf())
  expect_equal(s2$NETASS[s2$FILER_TYPE == "990EZ"], rep(60 - 30, 2))
  expect_true(all(s2$NETASS[s2$FILER_TYPE == "990"] == 999))
  expect_false(any(s$IMPUTED))
  expect_error(npc_standardize(fake_core(2019), source = "nccs_core"), "bmf")
  expect_error(npc_standardize(data.frame(ein = 1), source = "nccs_core", bmf = fake_bmf()),
               "missing columns")
})

test_that("npc_nccs_download caches files and logs hashes", {
  fb <- fake_bucket()
  withr_opts <- options(npcompete.nccs_url = fb$url)
  on.exit(options(withr_opts))
  key <- npcompete:::nccs_core_key(2019)
  p <- suppressMessages(npc_nccs_download(key, dir = fb$cache))
  expect_true(file.exists(p))
  log <- utils::read.csv(file.path(fb$cache, "downloads.csv"))
  expect_equal(log$key, key)
  expect_equal(log$md5, unname(tools::md5sum(p)))
  expect_silent(npc_nccs_download(key, dir = fb$cache))     # cached: no download
  expect_error(suppressMessages(
    npc_nccs_download("no/such/file.parquet", dir = fb$cache, retries = 1)),
    "Could not download")
})

test_that("npc_build_nccs builds tax years from two CORE files each", {
  fb <- fake_bucket()
  old <- options(npcompete.nccs_url = fb$url)
  on.exit(options(old))

  suppressMessages(npc_build_nccs(2018:2019, dir = fb$cache))
  info <- npc_archive_info(fb$cache)
  expect_equal(info$year_counts$YEAR, 2018:2019)
  expect_equal(info$manifest$value[info$manifest$key == "source"], "nccs_core")

  o <- npc_get_orgs(2018:2019, source = "nccs", dir = fb$cache, subsection = NULL)
  # EIN 1 (June year end) appears in tax year 2018 from the 2019 file
  e1 <- o[o$EIN2 == "EIN-00-0000001", ]
  expect_equal(e1$TOTREV[e1$YEAR == 2018], 100 + 2019)
  expect_equal(e1$TOTREV[e1$YEAR == 2019], 100 + 2020)
  expect_equal(o$CBSA_FIPS[o$EIN2 == "EIN-00-0000001"][1], "15540")   # Burlington, VT
  expect_false(4L %in% npc_get_orgs(2019, source = "nccs", dir = fb$cache)$SUBSECTION)

  # missing years are built on demand; 2020 needs the partial 2021 file
  expect_message(o2 <- npc_get_orgs(2020, source = "nccs", dir = fb$cache),
                 "Building tax year")
  expect_setequal(unique(o2$YEAR), 2020L)
  yc <- npc_archive_info(fb$cache)$year_counts
  expect_equal(yc$YEAR, 2018:2020)
  expect_equal(yc$INCOMPLETE, c(FALSE, FALSE, TRUE))

  # no file for 2022, so tax year 2021 cannot be built
  expect_error(suppressMessages(npc_build_nccs(2021, dir = fb$cache)),
               "no CORE 990combined file for 2022")
  expect_error(npc_build_nccs(2005, dir = fb$cache), "2010 onward")
})

test_that("npc_build_metrics runs on a live build", {
  fb <- fake_bucket()
  old <- options(npcompete.nccs_url = fb$url)
  on.exit(options(old))
  expect_message(
    m <- npc_build_metrics(2018:2020, geo = "county", resources = "TOTREV",
                           metrics = c("HHI", "DENSITY_PER_100K"),
                           source = "nccs", dir = fb$cache),
    "incomplete data: 2020")
  expect_setequal(unique(m$YEAR), 2018:2019)
  mf <- attr(m, "manifest")
  expect_equal(mf$value[mf$key == "source"], "nccs")
})

test_that("npc_compare_sources reports agreement between sources", {
  fb <- fake_bucket()
  old <- options(npcompete.nccs_url = fb$url)
  on.exit(options(old))
  arch <- build_tmp_archive()
  cmp <- suppressMessages(npc_compare_sources(2019, archive_dir = arch,
                                              nccs_dir = fb$cache))
  expect_named(cmp, c("totals", "ein_match", "markets", "market_summary"))
  expect_setequal(cmp$totals$SOURCE, c("archive", "nccs"))
  expect_true(all(c("BOTH", "ARCHIVE_ONLY", "NCCS_ONLY", "SAME_TOTREV") %in%
                    names(cmp$ein_match)))
  expect_true(all(cmp$markets$GEO_LEVEL == "county"))
})

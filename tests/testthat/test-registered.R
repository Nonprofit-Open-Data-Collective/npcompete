# Registered and grassroots universes, built offline from the fake bucket.

with_bucket <- function(code) {
  fb <- fake_bucket()
  old <- options(npcompete.nccs_url = fb$url)
  on.exit(options(old))
  code(fb)
}

ft <- function(d, i) d$FILER_TYPE[d$EIN2 == sprintf("EIN-00-%07d", i)]

test_that("registry spans apply BMF vintages, revocations and reinstatements", {
  with_bucket(function(fb) {
    s <- suppressMessages(npc_registry_spans(dir = fb$cache))
    row <- function(i) s[s$EIN2 == sprintf("EIN-00-%07d", i), ]
    expect_true(is.na(row(1)$END))                     # still in the latest vintage
    expect_equal(row(1)$START, 1990L)                  # ruling year before first vintage
    expect_equal(row(9)$END, 2018L)                    # revoked 2019, before leaving the BMF
    expect_equal(row(11)$REVOCATION_YEAR, 2018L)
    expect_equal(row(11)$REINSTATEMENT_YEAR, 2020L)
    expect_true(is.na(row(11)$END))                    # reinstated: span stays open
    expect_true(row(8)$CHURCH)
    expect_equal(row(7)$COUNTY_FIPS, "50007")
  })
})

test_that("registered universe classifies filer types by year", {
  with_bucket(function(fb) {
    r19 <- suppressMessages(
      npc_get_orgs(2019, universe = "registered", source = "nccs", dir = fb$cache))
    expect_equal(anyDuplicated(r19$EIN2), 0L)
    expect_true(all(c("FILING_REQ_CODE", "CHURCH") %in% names(r19)))
    expect_equal(ft(r19, 2), "990")
    expect_equal(ft(r19, 3), "990EZ")
    expect_equal(ft(r19, 7), "990N")                   # BMF filing requirement 2
    expect_equal(ft(r19, 8), "NONFILER")               # church
    expect_equal(ft(r19, 10), "990N")                  # e-postcard for 2019
    expect_equal(ft(r19, 12), "990PF")                 # private foundation
    expect_true(r19$FOUNDATION[r19$EIN2 == "EIN-00-0000012"])
    expect_length(ft(r19, 9), 0)                       # revoked in 2019
    expect_length(ft(r19, 11), 0)                      # between revocation and reinstatement
    expect_true(all(is.na(r19$TOTREV[r19$FILER_TYPE %in% c("990N", "NONFILER")])))
    expect_equal(r19$TOTREV[r19$EIN2 == "EIN-00-0000002"], 250 + 2019)   # December year end

    r18 <- npc_get_orgs(2018, universe = "registered", source = "nccs", dir = fb$cache)
    expect_equal(ft(r18, 9), "NONFILER")               # still registered in 2018
    expect_equal(ft(r18, 10), "NONFILER")              # postcard is for 2019 only
    expect_length(ft(r18, 11), 0)                      # revoked 2018

    g <- npc_get_orgs(2019, universe = "grassroots", source = "nccs", dir = fb$cache)
    expect_setequal(g$FILER_TYPE, c("990N", "NONFILER"))   # no foundations
    noch <- npc_get_orgs(2019, universe = "grassroots", source = "nccs",
                         dir = fb$cache, exclude_churches = TRUE)
    expect_false(any(noch$CHURCH))
    expect_equal(nrow(noch), nrow(g) - 1)

    yc <- utils::read.csv(file.path(fb$cache, "registered-nccs", "year-counts.csv"))
    expect_equal(yc$N[yc$YEAR == 2019], nrow(npc_get_orgs(2019, universe = "registered",
                                                          source = "nccs", dir = fb$cache,
                                                          subsection = NULL)))
    expect_error(npc_get_orgs(2019, universe = "registered", source = "nccs",
                              dir = fb$cache, cbsa = "geocode"), "geocoded CBSA")
  })
})

test_that("grassroots share and count metrics work for the registered universe", {
  d <- data.frame(GEO = c("A", "A", "A", "B"), SUB = "ART", YEAR = 2020,
                  FILER_TYPE = c("990", "990N", "NONFILER", "990EZ"))
  g <- get_grassroots_share(d, geography = "GEO", subsector = "SUB", year = "YEAR")
  a <- g[g$GEO == "A" & g$SUB == "ALL", ]
  expect_equal(a$N_GRASSROOTS, 2)
  expect_equal(a$GRASSROOTS_SHARE, 2 / 3)
  expect_equal(g$GRASSROOTS_SHARE[g$GEO == "ALL" & g$SUB == "ALL"], 0.5)

  expect_true("GRASSROOTS_SHARE" %in% npc_metrics("registered")$id)
  expect_false("GRASSROOTS_SHARE" %in% npc_metrics("filers")$id)
  expect_false("HHI" %in% npc_metrics("grassroots")$id)

  with_bucket(function(fb) {
    m <- suppressMessages(
      npc_build_metrics(2018:2019, geo = "county", universe = "registered",
                        source = "nccs", dir = fb$cache,
                        metrics = c("DENSITY_PER_100K", "GRASSROOTS_SHARE", "MEDIAN_AGE")))
    expect_setequal(unique(m$UNIVERSE), "registered")
    vt <- m[m$GEO == "50007" & m$SUB == "ALL" & m$YEAR == 2019, ]
    expect_equal(vt$VALUE[vt$METRIC == "N"], 4)        # EIN 1 (990), 7 (990N), 8 (church), 12 (990PF)
    expect_equal(vt$VALUE[vt$METRIC == "GRASSROOTS_SHARE"], 2 / 4)
    expect_error(npc_build_metrics(2019, universe = "grassroots", metrics = "HHI",
                                   source = "nccs", dir = fb$cache),
                 "requires financial data")
  })
})

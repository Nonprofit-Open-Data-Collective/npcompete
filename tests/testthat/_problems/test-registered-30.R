# Extracted from test-registered.R:30

# prequel ----------------------------------------------------------------------
with_bucket <- function(code) {
  fb <- fake_bucket()
  old <- options(npcompete.nccs_url = fb$url)
  on.exit(options(old))
  code(fb)
}
ft <- function(d, i) d$FILER_TYPE[d$EIN2 == sprintf("EIN-00-%07d", i)]

# test -------------------------------------------------------------------------
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
    expect_length(ft(r19, 9), 0)                       # revoked in 2019
    expect_length(ft(r19, 11), 0)                      # between revocation and reinstatement
    expect_true(all(is.na(r19$TOTREV[r19$FILER_TYPE %in% c("990N", "NONFILER")])))
    expect_equal(r19$TOTREV[r19$EIN2 == "EIN-00-0000002"], 250 + 2020)

    r18 <- npc_get_orgs(2018, universe = "registered", source = "nccs", dir = fb$cache)
    expect_equal(ft(r18, 9), "NONFILER")               # still registered in 2018
    expect_equal(ft(r18, 10), "NONFILER")              # postcard is for 2019 only
    expect_length(ft(r18, 11), 0)                      # revoked 2018

    g <- npc_get_orgs(2019, universe = "grassroots", source = "nccs", dir = fb$cache)
    expect_setequal(g$FILER_TYPE, c("990N", "NONFILER"))
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

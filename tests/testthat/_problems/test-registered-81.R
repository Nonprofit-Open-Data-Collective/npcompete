# Extracted from test-registered.R:81

# prequel ----------------------------------------------------------------------
with_bucket <- function(code) {
  fb <- fake_bucket()
  old <- options(npcompete.nccs_url = fb$url)
  on.exit(options(old))
  code(fb)
}
ft <- function(d, i) d$FILER_TYPE[d$EIN2 == sprintf("EIN-00-%07d", i)]

# test -------------------------------------------------------------------------
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
    expect_equal(vt$VALUE[vt$METRIC == "N"], 3)        # EIN 1 (990), 7 (990N), 8 (church)
    expect_equal(vt$VALUE[vt$METRIC == "GRASSROOTS_SHARE"], 2 / 3)
    expect_error(npc_build_metrics(2019, universe = "grassroots", metrics = "HHI",
                                   source = "nccs", dir = fb$cache),
                 "requires financial data")
  })

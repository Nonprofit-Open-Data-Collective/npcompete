# Entry, exit, churn and graduation.

dyn_panel <- function() {
  # a: 2018-2021; b: 2018-2019 then gone; c: enters 2020; d: misses 2019;
  # e: moves from market X to Y in 2020.
  data.frame(
    EIN2 = c(rep("a", 4), "b", "b", "c", "c", "d", "d", "e", "e", "e"),
    GEO  = c(rep("X", 10), "X", "X", "Y"),
    SUB  = "ART",
    YEAR = c(2018:2021, 2018, 2019, 2020, 2021, 2018, 2020, 2018, 2019, 2020),
    stringsAsFactors = FALSE)
}

pick <- function(x, geo, yr, sub = "ALL") x[x$GEO == geo & x$SUB == sub & x$YEAR == yr, ]

test_that("entries, exits and rates follow organizations across years", {
  d <- get_dynamics(dyn_panel(), geography = "GEO", subsector = "SUB", year = "YEAR",
                    absent_years = 2)
  x19 <- pick(d, "X", 2019)
  expect_equal(x19$N_PREV, 4)                    # a b d e (d filled in 2019)
  expect_equal(x19$N_CUR, 4)
  expect_equal(c(x19$ENTRIES, x19$EXITS), c(0, 0))
  x20 <- pick(d, "X", 2020)
  expect_equal(x20$ENTRIES, 1)                   # c
  expect_equal(x20$EXITS, 1)                     # b
  expect_equal(x20$EXIT_RATE, 1 / 4)
  expect_equal(x20$CHURN_RATE, 2 / 4)
  expect_equal(x20$NET_GROWTH, (3 - 4) / 4)      # e moved to Y: neither entry nor exit
  y20 <- pick(d, "Y", 2020)
  expect_equal(c(y20$N_PREV, y20$ENTRIES, y20$N_CUR), c(0, 0, 1))
  expect_true(is.na(y20$ENTRY_RATE))             # no stock in t - 1
  nat20 <- pick(d, "ALL", 2020)
  expect_equal(c(nat20$ENTRIES, nat20$EXITS), c(1, 1))
  # 2021 is the last observed year: exits need 2 years of absence to confirm
  expect_true(is.na(pick(d, "X", 2021)$EXITS))
  expect_false(is.na(pick(d, "X", 2021)$ENTRIES))
  expect_false(2018 %in% d$YEAR)                 # no prior year to compare
})

test_that("absent_years decides whether a gap is an exit and re-entry", {
  d1 <- get_dynamics(dyn_panel(), geography = "GEO", subsector = "SUB", year = "YEAR",
                     absent_years = 1)
  expect_equal(pick(d1, "X", 2019)$EXITS, 1)     # d leaves in 2019 ...
  expect_equal(pick(d1, "X", 2020)$ENTRIES, 2)   # ... and returns in 2020, with c
  expect_false(is.na(pick(d1, "X", 2021)$EXITS)) # one year of absence is enough
  expect_error(get_dynamics(dyn_panel(), geography = "GEO", subsector = "SUB",
                            year = "YEAR", observed_years = c(2018, 2020)),
               "consecutive")
  expect_error(get_dynamics(dyn_panel(), geography = "GEO", subsector = "SUB",
                            year = "YEAR", absent_years = 0), "whole number")
})

test_that("PPA revocations are counted and can be dropped", {
  p <- data.frame(EIN2 = c("a", "a", "b", "b", "c"), GEO = "X", SUB = "ART",
                  YEAR = c(2010, 2011, 2010, 2011, 2010),
                  REVOCATION_YEAR = c(NA, NA, NA, NA, 2011))
  d <- get_dynamics(p, geography = "GEO", subsector = "SUB", year = "YEAR",
                    absent_years = 1)
  expect_equal(pick(d, "X", 2011)$EXITS, 1)
  expect_equal(pick(d, "X", 2011)$EXITS_PPA, 1)
  dd <- get_dynamics(p, geography = "GEO", subsector = "SUB", year = "YEAR",
                     absent_years = 1, drop_ppa_revocations = TRUE)
  expect_equal(pick(dd, "X", 2011)$EXITS, 0)
  expect_equal(pick(dd, "X", 2011)$EXIT_RATE, 0)
})

test_that("graduation and downgrade rates use filer types", {
  p <- data.frame(EIN2 = c("a", "a", "b", "b", "c", "c", "f", "f"), GEO = "X", SUB = "ART",
                  YEAR = rep(2019:2020, 4),
                  FILER_TYPE = c("990N", "990EZ", "NONFILER", "NONFILER",
                                 "990", "990N", "990PF", "990PF"))
  d <- get_dynamics(p, geography = "GEO", subsector = "SUB", year = "YEAR",
                    filer_type = "FILER_TYPE", absent_years = 1)
  x <- pick(d, "X", 2020)
  expect_equal(c(x$N_PREV_GRASSROOTS, x$GRADUATIONS, x$GRADUATION_RATE), c(2, 1, 0.5))
  expect_equal(c(x$N_PREV_FILERS, x$DOWNGRADES, x$DOWNGRADE_RATE), c(1, 1, 1))
  expect_equal(x$N_PREV, 4)                      # foundations are in the stock
})

test_that("dynamics metrics are registered but not in the default set", {
  reg <- npc_metrics()
  expect_true(all(c("ENTRY_RATE", "EXIT_RATE", "NET_GROWTH", "CHURN_RATE") %in% reg$id))
  expect_false(any(reg$default[reg$family == "dynamics"]))
  expect_false("ENTRY_RATE" %in% npcompete:::resolve_metrics("default"))
  expect_true("ENTRY_RATE" %in% npcompete:::resolve_metrics("all"))
  expect_true("GRADUATION_RATE" %in% npc_metrics("registered")$id)
  expect_error(npcompete:::resolve_metrics("GRADUATION_RATE", "filers"),
               "only for universe = \"registered\"")
})

test_that("npc_build_metrics reads extra years for dynamics and censors exits", {
  arch <- build_tmp_archive()                    # 2018-2020, partial 2021
  m <- suppressMessages(npc_build_metrics(2019:2020, geo = "county",
                                          metrics = c("ENTRY_RATE", "EXIT_RATE"),
                                          dir = arch, format = "wide"))$county
  expect_setequal(unique(m$YEAR), 2019:2020)
  nat <- m[m$GEO == "ALL" & m$SUB == "ALL", ]
  # 2018 was read as the prior year, so 2019 has a stock and rates
  expect_true(all(nat$N_PREV > 0))
  expect_false(is.na(nat$EXIT_RATE[nat$YEAR == 2019]))
  # 2021 is incomplete, so 2020 exits cannot be confirmed
  expect_true(is.na(nat$EXIT_RATE[nat$YEAR == 2020]))
  expect_true(all(c("N_PREV", "ENTRIES", "EXITS") %in% names(m)))
  expect_error(npc_build_metrics(2020, metrics = "ENTRY_RATE", window = 2, dir = arch),
               "window = 1")

  long <- suppressMessages(npc_build_metrics(2020, geo = "county", metrics = "ENTRY_RATE",
                                             dir = arch))
  expect_true(all(is.na(long$RESOURCE)))
  mf <- attr(long, "manifest")
  expect_equal(mf$value[mf$key == "absent_years"], "2")
})

test_that("registered universe dynamics include graduation", {
  fb <- fake_bucket()
  old <- options(npcompete.nccs_url = fb$url)
  on.exit(options(old))
  m <- suppressMessages(
    npc_build_metrics(2019, geo = "county", universe = "registered", source = "nccs",
                      dir = fb$cache, format = "wide",
                      metrics = c("EXIT_RATE", "GRADUATION_RATE")))$county
  nat <- m[m$GEO == "ALL" & m$SUB == "ALL", ]
  # EIN 9 is revoked in 2019; EIN 11 was already gone (revoked 2018)
  expect_equal(nat$EXITS, 1)                     # EIN 9
  expect_false(is.na(nat$GRADUATION_RATE))
})

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



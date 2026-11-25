# Live NCCS mode, tested offline against a fake bucket in a local folder.

# Unified BMF. EINs 1-6 file 990s (see fake_core()); EINs 7-12 do not:
#   7  990-N filer by BMF code (filing requirement 2)
#   8  church (filing requirement 6)
#   9  revoked in 2019, never reinstated; last BMF vintage 2020-04
#   10 filing requirement 1, but an e-postcard for tax year 2019
#   11 revoked in 2018, reinstated in 2020
#   12 private foundation (fake_bmf_with_pf())
fake_bmf <- function() {
  data.frame(
    EIN2 = sprintf("EIN-00-%07d", 1:11),
    geo_county = c("Chittenden County", "Washington County", "City of Baltimore",
                   "Baltimore County", NA, "Doña Ana County", "Chittenden County",
                   "Chittenden County", "Washington County", "Washington County",
                   "Chittenden County"),
    geo_state_abbr = c("VT", "VT", "MD", "MD", NA, "NM", "VT", "VT", "VT", "VT", "VT"),
    nteev2 = c("ART-A20-RG", "HMS-P20-RG", "EDU-B20-RG", "ART-A20-RG",
               "HMS-P20-RG", "ENV-C20-RG", "ART-A20-RG", "REL-X20-RG",
               "HMS-P20-RG", "ENV-C20-RG", "EDU-B20-RG"),
    subsection_code = "03",
    ruling_date = c("1990-05-01", "2001-01-01", "1900-01-01", "2015-03-01",
                    "1980-01-01", "2010-07-01", "2005-01-01", "1950-01-01",
                    "2000-01-01", "2000-01-01", "2000-01-01"),
    filing_requirement_code = c("1", "1", "1", "1", "1", "1", "2", "6", "1", "1", "1"),
    first_vintage_ym = c(rep("1995-08", 6), "2010-01", "1989-06", "1995-08",
                         "1995-08", "1995-08"),
    last_vintage_ym = c(rep("2026-07", 8), "2020-04", "2026-07", "2026-07"),
    bmf_vintage_ym = "2026-08",
    foundation_code = "15",
    pf_filing_requirement_code = "0",
    stringsAsFactors = FALSE
  )
}

# EIN 12, a private foundation, added for the registered-universe tests.
fake_bmf_with_pf <- function() {
  b <- fake_bmf()
  pf <- b[7, ]
  pf$EIN2 <- "EIN-00-0000012"
  pf$filing_requirement_code <- "0"
  pf$foundation_code <- "04"
  pf$pf_filing_requirement_code <- "1"
  rbind(b, pf)
}

fake_revocations <- function() {
  data.frame(ein = c("000000009", "000000011"),
             legal_name = c("NINE", "ELEVEN"),
             revocation_date = c("15-MAY-2019", "15-MAY-2018"),
             exemption_reinstatement_date = c("", "01-MAR-2020"))
}

fake_postcards <- function() {
  data.frame(ein = c("000000007", "000000010"), tax_year = c("2024", "2019"),
             legal_name = c("SEVEN", "TEN"))
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
  arrow::write_parquet(fake_bmf_with_pf(), file.path(b, npcompete:::nccs_bmf_key))
  for (fy in file_years) {
    key <- npcompete:::nccs_core_key(fy)
    dir.create(file.path(b, dirname(key)), recursive = TRUE)
    core <- if (fy %in% partial) fake_core(fy)[c(1, 4), ] else fake_core(fy, n_extra = 3)
    arrow::write_parquet(core, file.path(b, key))
  }
  ym <- format(Sys.Date(), "%Y-%m")
  for (x in list(list(npcompete:::nccs_revocation_key(ym), fake_revocations()),
                 list(npcompete:::nccs_postcard_key(ym), fake_postcards()))) {
    dir.create(file.path(b, dirname(x[[1]])), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(x[[2]], file.path(b, x[[1]]), row.names = FALSE)
  }
  url <- paste0("file:///", sub("^/", "", normalizePath(b, winslash = "/")))
  list(url = url, cache = file.path(root, "cache"))
}



###---------------------------------------------------
###   Registered and grassroots universes
###---------------------------------------------------
#
# The registered universe in tax year t is every organization on the IRS
# registry that year. It is built from:
#
#   Unified BMF     one record per EIN with the first and last BMF vintage
#                   it appeared in, ruling date, subsection, filing
#                   requirement code, geocoded county and NTEEV2.
#   Revocations     the IRS automatic revocation list (all-time), with
#                   revocation and reinstatement dates.
#   E-postcard      the latest Form 990-N filed by each organization.
#   Filer panel     Form 990 / 990-EZ filers for t, from the archive or a
#                   live NCCS build.
#
# Each organization-year is classified as 990, 990EZ (in the filer panel
# for t), 990N (an e-postcard for t, or a BMF filing requirement code that
# allows the 990-N), 990PF (a private foundation), or NONFILER.

nccs_revocation_key <- function(ym) sprintf("raw/revocations/%s-REVOCATIONS-ORGS.csv", ym)
nccs_postcard_key   <- function(ym) sprintf("raw/e-postcard/%s-E-POSTCARD.csv", ym)

registry_bmf_cols <- c("EIN2", "geo_county", "geo_state_abbr", "nteev2",
                       "ruling_date", "subsection_code", "filing_requirement_code",
                       "first_vintage_ym", "last_vintage_ym", "foundation_code",
                       "pf_filing_requirement_code")

grassroots_types <- c("990N", "NONFILER")


#' Build the registered universe of organizations
#'
#' Builds one row per organization and tax year for every organization on
#' the IRS registry, filers and non-filers alike, and classifies each
#' organization-year by what it filed. The result is written to
#' `dir/registered-<filer_source>/orgs/` as a Parquet dataset partitioned by
#' `YEAR` and read with `npc_get_orgs(universe = "registered")` or
#' `"grassroots"`, which call this function for missing years.
#'
#' @section Who is registered in year t:
#' The unified BMF records the first and last monthly BMF vintage in which
#' each EIN appeared. An organization is registered from its ruling year (or
#' its first BMF vintage, if earlier or the ruling year is unknown) through
#' the year of its last vintage. Organizations still in the latest vintage
#' stay registered. Years between an automatic revocation and a
#' reinstatement are dropped, and a revocation ends the span if it comes
#' before the organization left the BMF. NCCS kept only three BMF vintages
#' between August 2016 and January 2022, so exits in those years without a
#' revocation date are dated to the last vintage seen.
#'
#' Any organization in the filer panel for year `t` is registered in `t`,
#' even if its BMF span says otherwise.
#'
#' @section Filer types:
#' \describe{
#'   \item{`990`, `990EZ`}{In the filer panel for `t` (exact tax year).}
#'   \item{`990PF`}{A private foundation on the BMF (PF filing requirement
#'     code 1, or foundation code 2-4). Foundations file the 990-PF, which
#'     the filer panels do not cover, so they have no financial data here.}
#'   \item{`990N`}{Not a filer or foundation, and either filed a 990-N postcard for `t`
#'     or has a BMF filing requirement code of 2 (gross receipts normally
#'     under the 990-N threshold). The postcard file holds only each
#'     organization's latest filing, so earlier years rely on the BMF code.}
#'   \item{`NONFILER`}{Everyone else: churches not required to file
#'     (`CHURCH = TRUE`, filing requirement code 6), dormant organizations
#'     not yet revoked, and organizations missing from the filer panel.}
#' }
#' Imputed gap-year rows of the archive panel do not count as filings; they
#' are classified like other non-filers and keep `IMPUTED = TRUE`.
#'
#' Location, subsector, subsection and ruling year come from the BMF for
#' every row (from the filer panel only when the BMF has no record), so a
#' filer's county here can differ from its county in the archive.
#'
#' @param years Tax years, 2010 onward, within the filer panel's coverage.
#' @param filer_source Where the 990 / 990-EZ filers come from: `"nccs"` (a
#'   live build, see [npc_build_nccs()]) or `"archive"` (the SOI panel
#'   archive).
#' @param dir Cache directory; see [npc_cache_dir()]. Downloads and the
#'   registered dataset go here.
#' @param filer_dir Directory of the filer panel: the archive directory for
#'   `filer_source = "archive"` (default [npc_archive_dir()]), or a live
#'   build (default `dir`).
#' @param refresh Download the BMF, revocation and e-postcard files again.
#'
#' @return The output directory, invisibly. It holds the Parquet dataset,
#'   `year-counts.csv` (organizations per year by filer type) and
#'   `manifest.csv`.
#' @seealso [npc_get_orgs()], [npc_registry_spans()]
#' @examples
#' \dontrun{
#' npc_build_registered( 2018:2020 )
#' g <- npc_get_orgs( 2019, universe = "grassroots", source = "nccs" )
#' table( g$FILER_TYPE )
#' }
#' @export
npc_build_registered <- function(years, filer_source = c("nccs", "archive"),
                                 dir = npc_cache_dir(), filer_dir = NULL,
                                 refresh = FALSE) {
  need_pkg("arrow", "read and write Parquet files")
  need_pkg("data.table", "read the revocation and e-postcard files")
  filer_source <- match.arg(filer_source)
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0 || any(is.na(years)) || any(years < 2010))
    stop("`years` must be tax years from 2010 onward.", call. = FALSE)
  if (is.null(filer_dir))
    filer_dir <- if (filer_source == "nccs") dir else npc_archive_dir()
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  spans <- npc_registry_spans(dir = dir, refresh = refresh)
  pc_path <- latest_nccs_file(nccs_postcard_key, dir, refresh)
  message("Reading the e-postcard file ...")
  pc <- data.table::fread(pc_path, select = c("ein", "tax_year"),
                          colClasses = "character", showProgress = FALSE)
  pc <- data.frame(EIN2 = ein_to_ein2(pc$ein),
                   PC_YEAR = suppressWarnings(as.integer(pc$tax_year)))
  pc <- pc[order(pc$EIN2, -pc$PC_YEAR), ]
  pc <- pc[!duplicated(pc$EIN2), ]
  spans$PC_YEAR <- pc$PC_YEAR[match(spans$EIN2, pc$EIN2)]

  target <- registered_dir(dir, filer_source)
  dir.create(target, showWarnings = FALSE, recursive = TRUE)
  counts <- list()
  for (t in years) {
    message("Building the registered universe for ", t, " ...")
    filers <- npc_get_orgs(t, universe = "filers", source = filer_source,
                           dir = filer_dir, subsection = NULL,
                           include_imputed = TRUE)
    reg <- registered_year(spans, filers, t)
    part <- file.path(target, "orgs", paste0("YEAR=", t))
    unlink(part, recursive = TRUE)
    dir.create(part, recursive = TRUE)
    arrow::write_parquet(reg[setdiff(names(reg), "YEAR")],
                         file.path(part, "part-0.parquet"))
    tab <- table(factor(reg$FILER_TYPE, c("990", "990EZ", "990PF", "990N", "NONFILER")))
    counts[[length(counts) + 1]] <- data.frame(
      YEAR = t, N = nrow(reg), N_990 = tab[["990"]], N_990EZ = tab[["990EZ"]],
      N_990PF = tab[["990PF"]],
      N_990N = tab[["990N"]], N_NONFILER = tab[["NONFILER"]],
      N_CHURCH = sum(reg$CHURCH), BUILT = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  }
  counts <- do.call(rbind, counts)

  yc_file <- file.path(target, "year-counts.csv")
  if (file.exists(yc_file)) {
    old <- utils::read.csv(yc_file, stringsAsFactors = FALSE)
    cols <- names(counts)
    counts <- dplyr::bind_rows(old[!old$YEAR %in% years, intersect(names(old), cols), drop = FALSE], counts)[cols]
  }
  counts <- counts[order(counts$YEAR), ]
  utils::write.csv(counts, yc_file, row.names = FALSE)

  logged <- function(key) {
    i <- download_info(dir, key)
    if (is.null(i) || nrow(i) == 0) NA_character_ else i$md5
  }
  manifest <- data.frame(
    key = c("universe", "source", "filer_source", "filer_dir", "bmf_md5",
            "revocations_file", "postcard_file", "years", "built", "npcompete_version"),
    value = c("registered", "bmf_registry", filer_source,
              normalizePath(filer_dir, winslash = "/", mustWork = FALSE),
              logged(nccs_bmf_key), attr(spans, "revocations_key"),
              sub(".*raw/", "raw/", gsub("\\\\", "/", pc_path)),
              paste(range(counts$YEAR), collapse = "-"),
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              as.character(utils::packageVersion("npcompete"))))
  utils::write.csv(manifest, file.path(target, "manifest.csv"), row.names = FALSE)
  message("Done: tax years ", paste(range(years), collapse = "-"), ".")
  invisible(target)
}


#' Registration spans from the BMF and the revocation list
#'
#' One row per EIN on the unified BMF with the years it was registered:
#' `START` (ruling year or first BMF vintage, whichever is earlier), `END`
#' (year of the last BMF vintage, or `NA` if still listed), and the latest
#' automatic revocation and reinstatement. Used by
#' [npc_build_registered()]; see there for the rules.
#'
#' @inheritParams npc_build_registered
#' @return A data frame with `EIN2`, `COUNTY_FIPS`, `NTEEV2`, `SUBSECTION`,
#'   `RULING_YEAR`, `FILING_REQ_CODE`, `CHURCH`, `FOUNDATION`, `START`, `END`,
#'   `REVOCATION_YEAR` and `REINSTATEMENT_YEAR`.
#' @export
npc_registry_spans <- function(dir = npc_cache_dir(), refresh = FALSE) {
  need_pkg("arrow", "read the BMF")
  need_pkg("data.table", "read the revocation file")
  bmf_path <- npc_nccs_download(nccs_bmf_key, dir, refresh = refresh)
  message("Reading the unified BMF ...")
  b <- read_parquet_quiet(bmf_path, registry_bmf_cols)
  b <- b[!duplicated(b$EIN2), ]

  int <- function(x) suppressWarnings(as.integer(x))
  ruling <- int(substr(b$ruling_date, 1, 4))
  ruling[ruling %in% 1900L] <- NA
  first <- int(substr(b$first_vintage_ym, 1, 4))
  last_ym <- as.character(b$last_vintage_ym)
  latest <- max(last_ym, na.rm = TRUE)
  frc <- trimws(as.character(b$filing_requirement_code))

  out <- data.frame(
    EIN2            = b$EIN2,
    COUNTY_FIPS     = npc_match_county(b$geo_county, b$geo_state_abbr),
    NTEEV2          = as.character(b$nteev2),
    SUBSECTION      = int(b$subsection_code),
    RULING_YEAR     = ruling,
    FILING_REQ_CODE = frc,
    CHURCH          = substr(frc, 1, 1) %in% "6",
    FOUNDATION      = trimws(as.character(b$pf_filing_requirement_code)) %in% "1" |
                      suppressWarnings(as.integer(b$foundation_code)) %in% 2:4,
    START           = pmin(ruling, first, na.rm = TRUE),
    END             = ifelse(last_ym %in% latest, NA_integer_, int(substr(last_ym, 1, 4))),
    stringsAsFactors = FALSE)

  rv_path <- latest_nccs_file(nccs_revocation_key, dir, refresh)
  message("Reading the revocation list ...")
  rv <- data.table::fread(rv_path, select = c("ein", "revocation_date",
                                              "exemption_reinstatement_date"),
                          colClasses = "character", showProgress = FALSE)
  year_of <- function(d) int(substr(d, nchar(d) - 3, nchar(d)))   # "15-JUL-2026"
  rv <- data.frame(EIN2 = ein_to_ein2(rv$ein),
                   REV = year_of(rv$revocation_date),
                   REIN = year_of(rv$exemption_reinstatement_date))
  rv <- rv[!is.na(rv$REV), ]
  rv <- rv[order(rv$EIN2, -rv$REV), ]
  rv <- rv[!duplicated(rv$EIN2), ]                       # latest revocation
  k <- match(out$EIN2, rv$EIN2)
  out$REVOCATION_YEAR <- rv$REV[k]
  out$REINSTATEMENT_YEAR <- rv$REIN[k]
  out$REINSTATEMENT_YEAR[!is.na(out$REINSTATEMENT_YEAR) &
                           out$REINSTATEMENT_YEAR < out$REVOCATION_YEAR] <- NA

  # A revocation without reinstatement ends the span if it comes first.
  ends <- !is.na(out$REVOCATION_YEAR) & is.na(out$REINSTATEMENT_YEAR)
  cut <- ends & (is.na(out$END) | out$REVOCATION_YEAR <= out$END)
  out$END[cut] <- out$REVOCATION_YEAR[cut] - 1L

  attr(out, "revocations_key") <- sub(".*raw/", "raw/", gsub("\\\\", "/", rv_path))
  attr(out, "latest_vintage") <- latest
  out
}


# Organization-years registered in tax year t, classified by filer type.
registered_year <- function(spans, filers, t) {
  s <- spans
  start <- ifelse(is.na(s$START), -Inf, s$START)
  end <- ifelse(is.na(s$END), Inf, s$END)
  gap <- !is.na(s$REVOCATION_YEAR) & !is.na(s$REINSTATEMENT_YEAR) &
         t >= s$REVOCATION_YEAR & t < s$REINSTATEMENT_YEAR
  active <- start <= t & t <= end & !gap

  real <- filers[!filers$IMPUTED %in% TRUE, ]
  real <- real[!duplicated(real$EIN2), ]
  ein <- union(s$EIN2[active], real$EIN2)

  k <- match(ein, s$EIN2)
  f <- match(ein, real$EIN2)
  fi <- match(ein, filers$EIN2)                          # includes imputed rows
  pick <- function(a, b) ifelse(is.na(a), b, a)

  out <- data.frame(
    EIN2               = ein,
    YEAR               = as.integer(t),
    COUNTY_FIPS        = pick(s$COUNTY_FIPS[k], filers$COUNTY_FIPS[fi]),
    CBSA_FIPS_GEOCODE  = NA_character_,
    NTEEV2             = pick(s$NTEEV2[k], filers$NTEEV2[fi]),
    SUBSECTION         = pick(s$SUBSECTION[k], filers$SUBSECTION[fi]),
    FILER_TYPE         = real$FILER_TYPE[f],
    EFILE              = real$EFILE[f],
    RULING_YEAR        = pick(s$RULING_YEAR[k], filers$RULING_YEAR[fi]),
    REVOKED            = s$REVOCATION_YEAR[k] %in% t,
    REVOCATION_YEAR    = s$REVOCATION_YEAR[k],
    REINSTATEMENT_YEAR = s$REINSTATEMENT_YEAR[k],
    stringsAsFactors   = FALSE)
  for (v in c("TOTREV", "TOTEXP", "NETASS", "TOTASS", "TOTLIA", "REVPROG",
              "REVCONT", "REVINVE", "FNDFEE", "FISYEAR", "FISMO"))
    out[[v]] <- real[[v]][f]
  out$IMPUTED <- !is.na(fi) & is.na(f)
  out$SOURCE <- ifelse(is.na(f), "BMF", real$SOURCE[f])

  nonfiler <- is.na(out$FILER_TYPE)
  pc_year <- s$PC_YEAR[k]
  frc <- s$FILING_REQ_CODE[k]
  pf <- nonfiler & s$FOUNDATION[k] %in% TRUE
  out$FILER_TYPE[pf] <- "990PF"
  nonfiler <- nonfiler & !pf
  postcard <- nonfiler & ((pc_year %in% t) | substr(frc, 1, 1) %in% "2")
  out$FILER_TYPE[postcard] <- "990N"
  out$FILER_TYPE[nonfiler & !postcard] <- "NONFILER"
  out$FILING_REQ_CODE <- frc
  out$CHURCH <- s$CHURCH[k] %in% TRUE
  out$FOUNDATION <- s$FOUNDATION[k] %in% TRUE

  out <- out[order(out$EIN2), c(canonical_cols, "FILING_REQ_CODE", "CHURCH", "FOUNDATION")]
  rownames(out) <- NULL
  out
}

# Folder of a registered-universe build.
registered_dir <- function(dir, filer_source) {
  file.path(dir, paste0("registered-", filer_source))
}

# Download the most recent monthly file of a series, trying back a year.
latest_nccs_file <- function(key_fun, dir, refresh = FALSE) {
  months <- format(seq(as.Date(format(Sys.Date(), "%Y-%m-01")), by = "-1 month",
                       length.out = 13), "%Y-%m")
  if (!refresh) {
    for (ym in months) {
      p <- file.path(dir, "raw", key_fun(ym))
      if (file.exists(p)) return(p)
    }
  }
  for (ym in months) {
    p <- tryCatch(npc_nccs_download(key_fun(ym), dir, refresh = refresh,
                                    retries = 1, quiet = TRUE),
                  error = function(e) NULL)
    if (!is.null(p)) {
      message("Using ", key_fun(ym))
      return(p)
    }
  }
  stop("No NCCS file found for ", key_fun("YYYY-MM"), " in the last 13 months.",
       call. = FALSE)
}

# "010002847" or "01-0002847" -> "EIN-01-0002847"
ein_to_ein2 <- function(x) {
  d <- gsub("[^0-9]", "", as.character(x))
  d <- formatC(d, width = 9, flag = "0")
  d <- gsub(" ", "0", d)
  paste0("EIN-", substr(d, 1, 2), "-", substr(d, 3, 9))
}

###---------------------------------------------------
###   Live source: NCCS downloads, harmonized to the canonical schema
###---------------------------------------------------
#
# Live mode reads two NCCS products:
#
#   CORE 990combined  processed_merged/core/{Y}/990combined/core_{Y}_990combined.parquet
#                     one file per calendar year in which the fiscal period
#                     ended; Form 990 and 990-EZ filers of every subsection.
#   Unified BMF       geocoding/unified-bmf/latest/bmf_unified_geocoded.parquet
#                     one row per EIN: geocoded county, NTEEV2, ruling date.
#
# Tax years follow the IRS / SOI convention used by the archive: tax year t
# is the year the fiscal period began. A December year end in Y is tax year
# Y; any other year end in Y is tax year Y - 1. Tax year t therefore needs
# the CORE files for t and t + 1.

# CORE financial fields mapped to canonical columns. The 990combined files
# carry no professional fundraising fee field, so FNDFEE is NA.
nccs_core_map <- c(
  TOTREV  = "total_revenue",
  TOTEXP  = "total_expenses",
  NETASS  = "total_net_assets_eoy",
  TOTASS  = "total_assets_eoy",
  TOTLIA  = "total_liabilities_eoy",
  REVPROG = "program_service_revenue",
  REVCONT = "total_contributions",
  REVINVE = "investment_income"
)

nccs_core_cols <- c("ein", "tax_year", "tax_month", "tax_period",
                    "subsection_cd", "source_form", "efile_indicator",
                    "extract_year", unname(nccs_core_map))

nccs_bmf_cols <- c("EIN2", "geo_county", "geo_state_abbr", "nteev2",
                   "ruling_date", "bmf_vintage_ym")

nccs_bmf_key <- "geocoding/unified-bmf/latest/bmf_unified_geocoded.parquet"

nccs_core_key <- function(file_year) {
  sprintf("processed_merged/core/%d/990combined/core_%d_990combined.parquet",
          file_year, file_year)
}

# Base URL of the NCCS data bucket. Tests point it at a local folder.
nccs_base_url <- function() {
  getOption("npcompete.nccs_url", "https://nccsdata.s3.us-east-1.amazonaws.com")
}


#' Location of the NCCS download cache
#'
#' Live builds ([npc_build_nccs()], or `source = "nccs"` in [npc_get_orgs()]
#' and [npc_build_metrics()]) keep downloaded NCCS files and the harmonized
#' organization table here. The location is taken from
#' `options(npcompete.cache_dir = )`, then the `NPCOMPETE_CACHE` environment
#' variable, then `tools::R_user_dir("npcompete", "cache")`.
#'
#' @param dir Optional directory; returned as is when supplied.
#' @return A directory path.
#' @examples
#' npc_cache_dir()
#' @export
npc_cache_dir <- function(dir = NULL) {
  if (!is.null(dir)) return(dir)
  d <- getOption("npcompete.cache_dir", Sys.getenv("NPCOMPETE_CACHE", ""))
  if (!nzchar(d)) d <- tools::R_user_dir("npcompete", "cache")
  d
}


#' Download a file from the NCCS data bucket
#'
#' Downloads one file to `dir/raw/`, retrying on failure, and records the
#' URL, size, MD5 hash and time in `dir/downloads.csv`. A file already in the
#' cache is reused unless `refresh = TRUE`.
#'
#' @param key Path of the file in the bucket, for example
#'   `"processed_merged/core/2019/990combined/core_2019_990combined.parquet"`.
#' @param dir Cache directory; see [npc_cache_dir()].
#' @param refresh Download again even if the file is cached.
#' @param retries Number of attempts before giving up.
#' @param quiet Suppress progress messages.
#'
#' @return The local path of the file.
#' @examples
#' \dontrun{
#' npc_nccs_download( "harmonized/core/CORE-HRMN_dd.csv" )
#' }
#' @export
npc_nccs_download <- function(key, dir = npc_cache_dir(), refresh = FALSE,
                              retries = 3, quiet = FALSE) {
  dest <- file.path(dir, "raw", key)
  if (file.exists(dest) && !refresh) return(dest)

  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  url <- paste(sub("/$", "", nccs_base_url()), key, sep = "/")
  tmp <- paste0(dest, ".part")
  old <- options(timeout = max(3600, getOption("timeout")))
  on.exit({ options(old); unlink(tmp) }, add = TRUE)

  if (!quiet) message("Downloading ", url, " ...")
  err <- NULL
  for (i in seq_len(retries)) {
    err <- tryCatch({
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
      NULL
    }, error = function(e) conditionMessage(e),
       warning = function(w) conditionMessage(w))
    if (is.null(err) && file.exists(tmp) && file.size(tmp) > 0) break
    if (i < retries) Sys.sleep(2^(i - 1))
  }
  if (!is.null(err) || !file.exists(tmp))
    stop("Could not download ", url, " after ", retries, " attempt(s): ", err,
         call. = FALSE)
  file.rename(tmp, dest)

  log_download(dir, key, url, dest)
  dest
}

log_download <- function(dir, key, url, path) {
  f <- file.path(dir, "downloads.csv")
  row <- data.frame(key = key, url = url, bytes = file.size(path),
                    md5 = unname(tools::md5sum(path)),
                    downloaded = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  log <- if (file.exists(f)) utils::read.csv(f, stringsAsFactors = FALSE) else NULL
  log <- rbind(log[!log$key %in% key, , drop = FALSE], row)
  utils::write.csv(log, f, row.names = FALSE)
}

download_info <- function(dir, key) {
  f <- file.path(dir, "downloads.csv")
  if (!file.exists(f)) return(NULL)
  log <- utils::read.csv(f, stringsAsFactors = FALSE)
  log[log$key == key, , drop = FALSE]
}


#' Build the harmonized organization table from NCCS downloads
#'
#' Downloads the NCCS CORE 990combined files and the unified geocoded BMF
#' for the requested tax years, harmonizes them to the canonical schema with
#' [npc_standardize()], and writes them to `dir/filers/` as a Parquet
#' dataset partitioned by `YEAR`, the same layout as [npc_build_archive()].
#' Years already built are kept, so the table can be grown a few years at a
#' time. [npc_get_orgs()] and [npc_build_metrics()] with `source = "nccs"`
#' call this for any year they need that is not built yet.
#'
#' Tax year `t` combines December year ends from CORE file `t` with other
#' year ends from file `t + 1`, so it needs both files. A tax year whose
#' `t + 1` file holds less than 80% of the rows of file `t` is flagged
#' incomplete (see [npc_archive_info()]).
#'
#' Location and subsector come from the unified BMF, one record per EIN, so
#' an organization that moved is placed at its latest address in every year.
#' The county is found by matching the geocoded county and state names to
#' [pop_county] (see [npc_match_county()]).
#'
#' @param years Integer vector of tax years. NCCS publishes 990combined files
#'   from 2010, so the earliest full tax year is 2010.
#' @param dir Cache directory; see [npc_cache_dir()]. Any folder works, so a
#'   live build can be kept as a frozen archive.
#' @param refresh Download the NCCS files again and rebuild the requested
#'   years even if they are cached.
#' @param keep_raw Keep the downloaded CORE files after the build. The BMF
#'   is always kept.
#'
#' @return The cache directory, invisibly. It holds `filers/`,
#'   `year-counts.csv` (rows per tax year, with the files and BMF used),
#'   `manifest.csv` and `downloads.csv` (URL, size and MD5 of every file).
#' @examples
#' \dontrun{
#' npc_build_nccs( 2018:2020 )
#' npc_archive_info( npc_cache_dir() )
#' }
#' @export
npc_build_nccs <- function(years, dir = npc_cache_dir(), refresh = FALSE,
                           keep_raw = TRUE) {

  need_pkg("arrow", "read NCCS Parquet files")
  years <- sort(unique(as.integer(years)))
  if (any(is.na(years)) || length(years) == 0)
    stop("`years` must be whole numbers.", call. = FALSE)
  if (any(years < 2010))
    stop("NCCS 990combined files start in 2010, so live builds cover tax ",
         "years 2010 onward. Requested: ", paste(years[years < 2010], collapse = ", "),
         ".", call. = FALSE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  if (any(years <= 2011))
    message("Tax years 2010-2011 come from NCCS legacy CORE files, which hold ",
            "fewer returns than later years and no form type (inferred from the ",
            "990-EZ limits). Counts, and dynamics across 2011-2012, are affected.")

  # --- BMF: geography, subsector, ruling year -------------------------------
  bmf_path <- npc_nccs_download(nccs_bmf_key, dir, refresh = refresh)
  message("Reading the unified BMF ...")
  bmf <- read_parquet_quiet(bmf_path, nccs_bmf_cols)
  bmf_md5 <- download_info(dir, nccs_bmf_key)$md5
  bmf_vintage <- suppressWarnings(max(bmf$bmf_vintage_ym, na.rm = TRUE))

  # --- CORE files, one per fiscal-year-end year ------------------------------
  file_years <- sort(unique(c(years, years + 1L)))
  file_rows <- stats::setNames(integer(length(file_years)), file_years)
  core <- vector("list", length(file_years))
  for (i in seq_along(file_years)) {
    fy <- file_years[i]
    key <- nccs_core_key(fy)
    path <- tryCatch(npc_nccs_download(key, dir, refresh = refresh),
                     error = function(e) {
                       stop("NCCS has no CORE 990combined file for ", fy,
                            ", which tax year ", fy - 1L, " needs. ",
                            conditionMessage(e), call. = FALSE)
                     })
    x <- read_parquet_quiet(path, nccs_core_cols)
    file_rows[i] <- nrow(x)
    x$FILE_YEAR <- fy
    core[[i]] <- x
    if (!keep_raw) unlink(path)
  }
  core <- as.data.frame(data.table::rbindlist(core, use.names = TRUE, fill = TRUE))

  message("Harmonizing ", format(nrow(core), big.mark = ","), " CORE rows ...")
  dat <- npc_standardize(core, source = "nccs_core", bmf = bmf)
  dat <- dat[dat$YEAR %in% years, ]

  # --- Write the requested year partitions -----------------------------------
  target <- file.path(dir, "filers")
  for (y in years) unlink(file.path(target, paste0("YEAR=", y)), recursive = TRUE)
  arrow::write_dataset(dat, target, format = "parquet", partitioning = "YEAR")

  # --- Year counts and manifest ----------------------------------------------
  n <- as.vector(table(factor(dat$YEAR, levels = years)))
  nxt <- file_rows[as.character(years + 1L)] / file_rows[as.character(years)]
  counts <- data.frame(
    YEAR = years, N = n,
    INCOMPLETE = unname(nxt < 0.8),
    CORE_FILES = paste(years, years + 1L, sep = ","),
    CORE_ROWS = paste(file_rows[as.character(years)],
                      file_rows[as.character(years + 1L)], sep = ","),
    BMF_MD5 = bmf_md5, BMF_VINTAGE = bmf_vintage,
    BUILT = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE)
  yc_file <- file.path(dir, "year-counts.csv")
  if (file.exists(yc_file)) {
    old <- utils::read.csv(yc_file, stringsAsFactors = FALSE)
    old <- old[!old$YEAR %in% years, intersect(names(counts), names(old)), drop = FALSE]
    counts <- dplyr::bind_rows(old, counts)
  }
  counts <- counts[order(counts$YEAR), ]
  utils::write.csv(counts, yc_file, row.names = FALSE)

  manifest <- data.frame(
    key = c("universe", "source", "source_file", "bmf_file", "bmf_md5",
            "bmf_vintage", "rows", "years", "built", "npcompete_version"),
    value = c("filers", "nccs_core", nccs_base_url(),
              nccs_bmf_key, bmf_md5, bmf_vintage, sum(counts$N),
              paste(range(counts$YEAR), collapse = "-"),
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              as.character(utils::packageVersion("npcompete"))))
  utils::write.csv(manifest, file.path(dir, "manifest.csv"), row.names = FALSE)

  if (any(counts$INCOMPLETE[counts$YEAR %in% years]))
    message("Incomplete tax year(s): ",
            paste(counts$YEAR[counts$INCOMPLETE & counts$YEAR %in% years], collapse = ", "),
            " (the following year's CORE file is still partial).")
  message("Done: ", format(sum(n), big.mark = ","), " rows for tax years ",
          paste(range(years), collapse = "-"), ".")
  invisible(dir)
}


# Map CORE 990combined rows plus the unified BMF to the canonical schema.
standardize_nccs_core <- function(df, bmf) {
  if (is.null(bmf))
    stop("`bmf` is needed to standardize NCCS CORE files: it supplies ",
         "county, NTEEV2 and ruling year.", call. = FALSE)
  need <- c("ein", "tax_year", "subsection_cd", unname(nccs_core_map))
  missing_cols <- setdiff(need, names(df))
  if (length(missing_cols) > 0)
    stop("Not an NCCS CORE 990combined layout; missing columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  missing_bmf <- setdiff(c("EIN2", "geo_county", "geo_state_abbr", "nteev2", "ruling_date"),
                         names(bmf))
  if (length(missing_bmf) > 0)
    stop("Not a unified BMF layout; missing columns: ",
         paste(missing_bmf, collapse = ", "), call. = FALSE)

  num <- function(x) suppressWarnings(as.numeric(x))
  int <- function(x) suppressWarnings(as.integer(x))
  col <- function(nm) if (nm %in% names(df)) df[[nm]] else rep(NA, nrow(df))

  tax_year  <- int(df$tax_year)
  tax_month <- int(col("tax_month"))
  period    <- as.character(col("tax_period"))
  tax_month[is.na(tax_month)] <- int(substr(period[is.na(tax_month)], 5, 6))
  year <- tax_year - (!is.na(tax_month) & tax_month < 12L)

  ein <- toupper(as.character(df$ein))
  ein2 <- ifelse(grepl("^EIN-", ein), ein, paste0("EIN-", ein))

  # The legacy files behind tax years 2010-2011 have no form type; infer it
  # from the 990-EZ limits, using revenue for gross receipts (under $200k,
  # with assets under $500k); this matches the reported form for 89% of
  # 2012-2013 returns.
  form <- tolower(as.character(col("source_form")))
  infer <- is.na(form) | form == ""
  small <- num(df$total_revenue) < 2e5 & num(df$total_assets_eoy) < 5e5
  form[infer] <- ifelse(small[infer] %in% TRUE, "990ez", "990")
  filer <- ifelse(form == "990", "990", ifelse(form == "990ez", "990EZ", NA_character_))
  ef <- col("efile_indicator")
  efile <- ifelse(is.na(ef), NA_character_,
                  ifelse(as.logical(ef), "EFILE", "PAPER"))

  b <- bmf[!duplicated(bmf$EIN2), ]
  b$COUNTY_FIPS <- npc_match_county(b$geo_county, b$geo_state_abbr)
  b$RULING_YEAR <- int(substr(as.character(b$ruling_date), 1, 4))
  b$RULING_YEAR[b$RULING_YEAR %in% 1900L] <- NA
  k <- match(ein2, b$EIN2)

  out <- data.frame(
    EIN2               = ein2,
    YEAR               = as.integer(year),
    COUNTY_FIPS        = b$COUNTY_FIPS[k],
    CBSA_FIPS_GEOCODE  = NA_character_,
    NTEEV2             = as.character(b$nteev2[k]),
    SUBSECTION         = int(df$subsection_cd),
    FILER_TYPE         = filer,
    EFILE              = efile,
    RULING_YEAR        = b$RULING_YEAR[k],
    REVOKED            = NA,
    REVOCATION_YEAR    = NA_integer_,
    REINSTATEMENT_YEAR = NA_integer_,
    stringsAsFactors   = FALSE
  )
  for (v in names(nccs_core_map)) out[[v]] <- num(df[[nccs_core_map[[v]]]])
  # 990-EZ net assets: use the balance sheet (assets - liabilities), as the
  # SOI panel does, rather than the Part I line, which often disagrees.
  ez <- out$FILER_TYPE %in% "990EZ" & !is.na(out$TOTASS) & !is.na(out$TOTLIA)
  out$NETASS[ez] <- out$TOTASS[ez] - out$TOTLIA[ez]
  out$FNDFEE   <- NA_real_
  out$FISYEAR  <- tax_year
  out$FISMO    <- tax_month
  out$IMPUTED  <- FALSE
  fy <- if ("FILE_YEAR" %in% names(df)) df$FILE_YEAR else tax_year
  out$SOURCE   <- paste0("NCCS_CORE_", fy, "_", ifelse(is.na(filer), "NA", filer))

  # One row per EIN and tax year: prefer the latest extract, then the latest
  # period end (amended and short-period returns), then the full 990.
  ord <- order(out$EIN2, out$YEAR,
               -num(col("extract_year")), -num(period), out$FILER_TYPE != "990",
               na.last = TRUE)
  out <- out[ord, ]
  out <- out[!duplicated(out[c("EIN2", "YEAR")]), ]
  rownames(out) <- NULL
  out[canonical_cols]
}


#' Match county names to county FIPS codes
#'
#' Finds the five-digit FIPS code for county names such as those returned by
#' a geocoder ("Cook County", "Norfolk city", "Lafourche Parish",
#' "Municipality of Anchorage", "City of Baltimore"). Names are matched
#' within the state against [pop_county], ignoring case, accents,
#' punctuation and the type of county. Where a state has both a county and
#' an independent city of the same name (Baltimore, St. Louis, Richmond VA,
#' ...), names marked as a city match the independent city.
#'
#' @param county County names.
#' @param state Two-letter state abbreviations, the same length as `county`.
#' @return A character vector of FIPS codes, `NA` where no county matched.
#' @examples
#' npc_match_county( c( "Cook County", "City of Baltimore", "Baltimore County",
#'                      "Doña Ana County" ),
#'                   c( "IL", "MD", "MD", "NM" ) )
#' @export
npc_match_county <- function(county, state) {
  if (length(state) == 1) state <- rep(state, length(county))
  stopifnot(length(county) == length(state))
  key <- paste(toupper(state), county, sep = "|")
  u <- unique(key[!is.na(county) & !is.na(state)])
  if (length(u) == 0) return(rep(NA_character_, length(county)))

  st <- sub("\\|.*$", "", u)
  nm <- sub("^[^|]*\\|", "", u)
  is_city <- grepl("(^| )city$", nm, ignore.case = TRUE) |
             grepl("^city of ", nm, ignore.case = TRUE)
  norm <- normalize_county(nm)

  pc <- npcompete::pop_county
  pc <- pc[!duplicated(pc$COUNTY_FIPS), c("COUNTY_FIPS", "STATE_FIPS", "COUNTY_NAME")]
  pc$NORM <- normalize_county(pc$COUNTY_NAME)
  pc$CITY <- as.integer(substr(pc$COUNTY_FIPS, 3, 5)) >= 500 &
             pc$STATE_FIPS %in% c("24", "29", "32", "51")      # independent cities
  pc$KEY <- paste(pc$STATE_FIPS, pc$NORM, sep = "|")

  fips_state <- state_fips[st]
  cand <- paste(fips_state, norm, sep = "|")
  hit <- vapply(seq_along(u), function(i) {
    m <- which(pc$KEY == cand[i])
    if (length(m) > 1) m <- m[pc$CITY[m] == is_city[i]]
    if (length(m) == 1) pc$COUNTY_FIPS[m] else NA_character_
  }, character(1))

  out <- hit[match(key, u)]
  out[is.na(county) | is.na(state)] <- NA_character_
  out
}

# Reduce a county name to a bare, comparable form.
normalize_county <- function(x) {
  x <- iconv(enc2utf8(as.character(x)), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- tolower(trimws(x))
  x <- gsub("[.'`~^\"]", "", x)
  x <- gsub("-", " ", x)
  x <- sub("^(city and county of|city and borough of|municipality of|city of|county of|the) ", "", x)
  x <- sub(" (county|parish|borough|census area|city and borough|municipality|municipio|city|planning region)$", "", x)
  x <- gsub("\\bsaint\\b", "st", x)
  x <- gsub("\\bsainte\\b", "ste", x)
  gsub("\\s+", " ", trimws(x))
}

# State and territory abbreviations to FIPS codes.
state_fips <- c(
  AL = "01", AK = "02", AZ = "04", AR = "05", CA = "06", CO = "08", CT = "09",
  DE = "10", DC = "11", FL = "12", GA = "13", HI = "15", ID = "16", IL = "17",
  IN = "18", IA = "19", KS = "20", KY = "21", LA = "22", ME = "23", MD = "24",
  MA = "25", MI = "26", MN = "27", MS = "28", MO = "29", MT = "30", NE = "31",
  NV = "32", NH = "33", NJ = "34", NM = "35", NY = "36", NC = "37", ND = "38",
  OH = "39", OK = "40", OR = "41", PA = "42", RI = "44", SC = "45", SD = "46",
  TN = "47", TX = "48", UT = "49", VT = "50", VA = "51", WA = "53", WV = "54",
  WI = "55", WY = "56", AS = "60", GU = "66", MP = "69", PR = "72", VI = "78"
)


#' Compare live NCCS filers with the local archive
#'
#' Reads the same tax years from the SOI panel archive and from a live NCCS
#' build and reports how far they agree: organization counts and resource
#' totals by year, counts by market, and an EIN-level match of financial
#' values and county. Imputed gap-year rows are left out of the archive side,
#' since live builds have none.
#'
#' @param years Tax years present in both sources.
#' @param archive_dir Archive directory; see [npc_archive_dir()].
#' @param nccs_dir Live build directory; see [npc_cache_dir()]. Missing years
#'   are built.
#' @param geo Market level for the by-market comparison: `"county"` or
#'   `"cbsa"`.
#' @param resources Resource columns to compare.
#' @inheritParams npc_get_orgs
#'
#' @return A list of data frames:
#' \describe{
#'   \item{totals}{By year and source: rows, distinct organizations and
#'     resource totals}
#'   \item{ein_match}{By year: EINs in both sources or only one, extra
#'     archive rows for an EIN already seen that year, and among EINs in
#'     both, the share with the same filer type, county, and each resource
#'     within $1}
#'   \item{markets}{By market and year: organizations in each source, the
#'     difference and the ratio}
#'   \item{market_summary}{By year: correlation of market counts and the
#'     median absolute percent difference, for markets with 10 or more
#'     archive organizations}
#' }
#' @examples
#' \dontrun{
#' cmp <- npc_compare_sources( 2017:2019 )
#' cmp$totals
#' cmp$ein_match
#' }
#' @export
npc_compare_sources <- function(years, archive_dir = npc_archive_dir(),
                                nccs_dir = npc_cache_dir(),
                                geo = c("county", "cbsa"),
                                resources = c("TOTREV", "TOTEXP", "NETASS", "TOTASS"),
                                subsection = 3) {
  geo <- match.arg(geo)
  cols <- c("EIN2", "YEAR", "COUNTY_FIPS", "CBSA_FIPS", "FILER_TYPE", resources)
  a <- npc_get_orgs(years, source = "archive", dir = archive_dir,
                    subsection = subsection, include_imputed = FALSE, columns = cols)
  n <- npc_get_orgs(years, source = "nccs", dir = nccs_dir,
                    subsection = subsection, columns = cols)
  a$SOURCE <- "archive"
  n$SOURCE <- "nccs"
  both <- rbind(a, n)

  totals <- stats::aggregate(
    both[resources], by = both[c("YEAR", "SOURCE")],
    FUN = function(x) sum(x, na.rm = TRUE))
  counts <- stats::aggregate(list(ROWS = both$EIN2), by = both[c("YEAR", "SOURCE")], FUN = length)
  uniq <- stats::aggregate(list(N = both$EIN2), by = both[c("YEAR", "SOURCE")],
                           FUN = function(x) length(unique(x)))
  totals <- merge(merge(counts, uniq, by = c("YEAR", "SOURCE")), totals, by = c("YEAR", "SOURCE"))
  totals <- totals[order(totals$YEAR, totals$SOURCE), c("YEAR", "SOURCE", "ROWS", "N", resources)]
  rownames(totals) <- NULL

  # The archive can hold more than one row for an EIN and year (short
  # periods after an accounting-period change); match on the first.
  a1 <- a[!duplicated(a[c("EIN2", "YEAR")]), ]
  m <- merge(a1, n, by = c("EIN2", "YEAR"), suffixes = c(".a", ".n"))
  same <- function(x, y) (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & abs(x - y) <= 1)
  ein_match <- do.call(rbind, lapply(years, function(y) {
    my <- m[m$YEAR == y, ]
    ay <- a$EIN2[a$YEAR == y]
    ny <- n$EIN2[n$YEAR == y]
    r <- data.frame(YEAR = y, BOTH = nrow(my),
                    ARCHIVE_ONLY = sum(!unique(ay) %in% ny), NCCS_ONLY = sum(!ny %in% ay),
                    ARCHIVE_DUP_ROWS = sum(duplicated(ay)),
                    SAME_FILER_TYPE = mean(my$FILER_TYPE.a == my$FILER_TYPE.n, na.rm = TRUE),
                    SAME_COUNTY = mean(my$COUNTY_FIPS.a == my$COUNTY_FIPS.n, na.rm = TRUE),
                    NCCS_NO_COUNTY = mean(is.na(n$COUNTY_FIPS[n$YEAR == y])))
    for (v in resources)
      r[[paste0("SAME_", v)]] <- mean(same(my[[paste0(v, ".a")]], my[[paste0(v, ".n")]]))
    r
  }))

  gcol <- c(county = "COUNTY_FIPS", cbsa = "CBSA_FIPS")[[geo]]
  cnt <- function(d) {
    d <- d[!is.na(d[[gcol]]), ]
    if (nrow(d) == 0) return(data.frame(GEO = character(), YEAR = integer(), N = integer()))
    stats::aggregate(list(N = d$EIN2), by = list(GEO = d[[gcol]], YEAR = d$YEAR),
                     FUN = function(x) length(unique(x)))
  }
  markets <- merge(cnt(a), cnt(n), by = c("GEO", "YEAR"), all = TRUE,
                   suffixes = c("_ARCHIVE", "_NCCS"))
  markets$N_ARCHIVE[is.na(markets$N_ARCHIVE)] <- 0L
  markets$N_NCCS[is.na(markets$N_NCCS)] <- 0L
  markets$DIFF <- markets$N_NCCS - markets$N_ARCHIVE
  markets$RATIO <- ifelse(markets$N_ARCHIVE > 0, markets$N_NCCS / markets$N_ARCHIVE, NA)
  markets$GEO_LEVEL <- geo
  markets <- markets[order(markets$YEAR, markets$GEO),
                     c("GEO_LEVEL", "GEO", "YEAR", "N_ARCHIVE", "N_NCCS", "DIFF", "RATIO")]
  rownames(markets) <- NULL

  big <- markets[markets$N_ARCHIVE >= 10, ]
  market_summary <- do.call(rbind, lapply(years, function(y) {
    b <- big[big$YEAR == y, ]
    data.frame(YEAR = y, MARKETS = nrow(b),
               COR_N = if (nrow(b) > 2) stats::cor(b$N_ARCHIVE, b$N_NCCS) else NA_real_,
               MEDIAN_ABS_PCT_DIFF = if (nrow(b)) stats::median(abs(b$RATIO - 1)) * 100 else NA_real_)
  }))

  list(totals = totals, ein_match = ein_match, markets = markets,
       market_summary = market_summary)
}

# Read selected columns of a Parquet file, without arrow's warning about R
# metadata that NCCS files carry from the pipeline that wrote them.
read_parquet_quiet <- function(path, cols) {
  withCallingHandlers(
    as.data.frame(arrow::read_parquet(path, col_select = dplyr::any_of(cols))),
    warning = function(w) {
      if (grepl("unsafe or invalid elements", conditionMessage(w)))
        invokeRestart("muffleWarning")
    })
}

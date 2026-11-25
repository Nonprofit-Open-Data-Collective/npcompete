###---------------------------------------------------
###   Local archive: canonical schema, Parquet storage, readers
###---------------------------------------------------

# Columns of the canonical organization-year schema, in order.
canonical_cols <- c(
  "EIN2", "YEAR", "COUNTY_FIPS", "CBSA_FIPS_GEOCODE", "NTEEV2", "SUBSECTION",
  "FILER_TYPE", "EFILE", "RULING_YEAR", "REVOKED", "REVOCATION_YEAR",
  "REINSTATEMENT_YEAR", "TOTREV", "TOTEXP", "NETASS", "TOTASS", "TOTLIA",
  "REVPROG", "REVCONT", "REVINVE", "FNDFEE", "FISYEAR", "FISMO", "IMPUTED",
  "SOURCE"
)

# Columns read from the SOI panel sample frame CSV.
soi_panel_cols <- c(
  "EIN2", "TAXYEAR", "COUNTY_FIPS", "CENSUS_CBSA_FIPS", "NTEEV2", "V501C",
  "SOURCE", "EFILE_X", "ORG_RULING_YEAR", "REVOKED_YEAR_X", "revocation_date",
  "exemption_reinstatement_date", "TOTREV", "TOTEXP", "NETASS", "TOTASS",
  "TOTLIA", "REVPROG", "REVCONT", "REVINVE", "FNDFEE", "FISYEAR", "FISMO",
  "MISSING_YEAR"
)


#' Convert an organization panel to the canonical npcompete schema
#'
#' Maps the columns of a source panel onto the organization-year schema that
#' the archive and [npc_build_metrics()] use, so that every data source ends
#' up in the same layout.
#'
#' @param df A data frame from the source panel.
#' @param source Which source layout `df` follows. Currently only
#'   `"soi_panel"`, the NCCS SOI panel sample frame of Form 990 and 990-EZ
#'   filers.
#'
#' @return A data frame with one row per organization-year and columns:
#' \describe{
#'   \item{EIN2}{Employer identification number (`EIN-XX-XXXXXXX`)}
#'   \item{YEAR}{Tax year}
#'   \item{COUNTY_FIPS}{Five-digit county FIPS code}
#'   \item{CBSA_FIPS_GEOCODE}{CBSA assigned when the address was geocoded;
#'     [npc_get_orgs()] adds `CBSA_FIPS` from the county crosswalk}
#'   \item{NTEEV2}{NTEE code in NCCS version 2 format}
#'   \item{SUBSECTION}{501(c) subsection}
#'   \item{FILER_TYPE}{`"990"`, `"990EZ"`, or `NA` for imputed gap years}
#'   \item{EFILE}{`"EFILE"`, `"PAPER"` or `NA`}
#'   \item{RULING_YEAR}{Year the IRS granted exemption}
#'   \item{REVOKED}{`TRUE` in the year the exemption was revoked}
#'   \item{REVOCATION_YEAR, REINSTATEMENT_YEAR}{Years of automatic revocation
#'     and reinstatement, if any}
#'   \item{TOTREV, TOTEXP, NETASS, TOTASS, TOTLIA}{Revenue, expenses, net
#'     assets, assets and liabilities}
#'   \item{REVPROG, REVCONT, REVINVE, FNDFEE}{Program revenue, contributions,
#'     investment income and professional fundraising fees}
#'   \item{FISYEAR, FISMO}{Fiscal year and month the tax period ended}
#'   \item{IMPUTED}{`TRUE` for rows that fill a gap between filings; they
#'     have no financial data}
#'   \item{SOURCE}{The source file the row came from}
#' }
#'
#' @examples
#' raw <- data.frame(
#'   EIN2 = "EIN-01-0000001", TAXYEAR = 2019, COUNTY_FIPS = 1001,
#'   CENSUS_CBSA_FIPS = 33860, NTEEV2 = "ART-A20-RG", V501C = 3,
#'   SOURCE = "SOI_2020_PC", EFILE_X = "EFILE", ORG_RULING_YEAR = 1990,
#'   REVOKED_YEAR_X = 0, revocation_date = "", exemption_reinstatement_date = "",
#'   TOTREV = 1000, TOTEXP = 900, NETASS = 50, TOTASS = 80, TOTLIA = 30,
#'   REVPROG = 100, REVCONT = 800, REVINVE = 10, FNDFEE = NA, FISYEAR = 2019,
#'   FISMO = 12, MISSING_YEAR = 0 )
#' npc_standardize( raw )
#'
#' @export
npc_standardize <- function(df, source = "soi_panel") {

  source <- match.arg(source)
  missing_cols <- setdiff(soi_panel_cols, names(df))
  if (length(missing_cols) > 0)
    stop("Not an SOI panel layout; missing columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)

  chr  <- function(x) { x <- as.character(x); x[x == ""] <- NA; x }
  num  <- function(x) suppressWarnings(as.numeric(x))
  int  <- function(x) suppressWarnings(as.integer(x))
  fips <- function(x, width) {
    x <- chr(x)
    ok <- !is.na(x)
    x[ok] <- formatC(int(x[ok]), width = width, flag = "0")
    x[x == "NA"] <- NA
    x
  }
  year_of <- function(d) int(substr(chr(d), 8, 11))      # "15-FEB-2019"

  src <- chr(df$SOURCE)
  filer <- ifelse(grepl("_PC$", src), "990",
                  ifelse(grepl("_EZ$", src), "990EZ", NA_character_))
  efile <- chr(df$EFILE_X)
  efile[!efile %in% c("EFILE", "PAPER")] <- NA

  out <- data.frame(
    EIN2               = chr(df$EIN2),
    YEAR               = int(df$TAXYEAR),
    COUNTY_FIPS        = fips(df$COUNTY_FIPS, 5),
    CBSA_FIPS_GEOCODE  = fips(df$CENSUS_CBSA_FIPS, 5),
    NTEEV2             = chr(df$NTEEV2),
    SUBSECTION         = int(df$V501C),
    FILER_TYPE         = filer,
    EFILE              = efile,
    RULING_YEAR        = int(df$ORG_RULING_YEAR),
    REVOKED            = int(df$REVOKED_YEAR_X) %in% 1L,
    REVOCATION_YEAR    = year_of(df$revocation_date),
    REINSTATEMENT_YEAR = year_of(df$exemption_reinstatement_date),
    TOTREV             = num(df$TOTREV),
    TOTEXP             = num(df$TOTEXP),
    NETASS             = num(df$NETASS),
    TOTASS             = num(df$TOTASS),
    TOTLIA             = num(df$TOTLIA),
    REVPROG            = num(df$REVPROG),
    REVCONT            = num(df$REVCONT),
    REVINVE            = num(df$REVINVE),
    FNDFEE             = num(df$FNDFEE),
    FISYEAR            = int(df$FISYEAR),
    FISMO              = int(df$FISMO),
    IMPUTED            = int(df$MISSING_YEAR) %in% 1L,
    SOURCE             = src,
    stringsAsFactors   = FALSE
  )
  out$RULING_YEAR[out$RULING_YEAR %in% 0L] <- NA
  out[canonical_cols]
}


#' Location of the local data archive
#'
#' The archive directory is taken from `options(npcompete.archive_dir = )`,
#' then the `NPCOMPETE_ARCHIVE` environment variable.
#'
#' @param dir Optional directory; returned as is when supplied.
#' @return A directory path.
#' @examples
#' \dontrun{
#' options( npcompete.archive_dir = "D:/data/npcompete-archive" )
#' npc_archive_dir()
#' }
#' @export
npc_archive_dir <- function(dir = NULL) {
  if (!is.null(dir)) return(dir)
  d <- getOption("npcompete.archive_dir", Sys.getenv("NPCOMPETE_ARCHIVE", ""))
  if (!nzchar(d))
    stop("No archive directory set. Use options(npcompete.archive_dir = ...) ",
         "or the NPCOMPETE_ARCHIVE environment variable, or pass `dir`.",
         call. = FALSE)
  d
}


#' Build the local Parquet archive from the SOI panel CSV
#'
#' Reads the SOI panel sample frame CSV once, converts it to the canonical
#' schema with [npc_standardize()], and writes it to `dir/filers/` as a
#' Parquet dataset partitioned by `YEAR`. Later reads with [npc_get_orgs()]
#' only touch the years requested. A `manifest.csv` and `year-counts.csv`
#' record what was built.
#'
#' @param csv Path to `SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED*.CSV`.
#' @param dir Archive directory; see [npc_archive_dir()].
#' @param overwrite Replace an existing `filers` dataset.
#'
#' @return The archive directory, invisibly.
#' @examples
#' \dontrun{
#' npc_build_archive( "SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED-W-POP.CSV",
#'                    dir = "D:/data/npcompete-archive" )
#' }
#' @export
npc_build_archive <- function(csv, dir = npc_archive_dir(), overwrite = FALSE) {

  need_pkg("arrow", "build the Parquet archive")
  need_pkg("data.table", "read the source CSV")

  target <- file.path(dir, "filers")
  if (dir.exists(target)) {
    if (!overwrite) stop("Archive already exists at ", target,
                         "; use overwrite = TRUE to replace it.", call. = FALSE)
    unlink(target, recursive = TRUE)
  }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  message("Reading ", basename(csv), " ...")
  raw <- data.table::fread(
    csv, select = soi_panel_cols, showProgress = FALSE,
    colClasses = list(character = c("COUNTY_FIPS", "CENSUS_CBSA_FIPS",
                                    "revocation_date",
                                    "exemption_reinstatement_date")))

  message("Standardizing ", format(nrow(raw), big.mark = ","), " rows ...")
  dat <- npc_standardize(as.data.frame(raw))
  rm(raw)
  dat <- dat[!is.na(dat$YEAR), ]

  message("Writing Parquet dataset to ", target, " ...")
  arrow::write_dataset(dat, target, format = "parquet", partitioning = "YEAR")

  counts <- as.data.frame(table(YEAR = dat$YEAR), stringsAsFactors = FALSE)
  names(counts)[2] <- "N"
  counts$YEAR <- as.integer(counts$YEAR)
  utils::write.csv(counts, file.path(dir, "year-counts.csv"), row.names = FALSE)

  manifest <- data.frame(
    key = c("universe", "source", "source_file", "source_size_bytes",
            "source_modified", "rows", "years", "built", "npcompete_version"),
    value = c("filers", "soi_panel", normalizePath(csv, winslash = "/"),
              format(file.size(csv), scientific = FALSE),
              format(file.mtime(csv), "%Y-%m-%d %H:%M:%S"),
              nrow(dat), paste(range(dat$YEAR), collapse = "-"),
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              as.character(utils::packageVersion("npcompete"))))
  utils::write.csv(manifest, file.path(dir, "manifest.csv"), row.names = FALSE)

  message("Done: ", format(nrow(dat), big.mark = ","), " rows, years ",
          paste(range(dat$YEAR), collapse = "-"), ".")
  invisible(dir)
}


#' Describe the local archive
#'
#' @inheritParams npc_build_archive
#' @return A list with `manifest` (key/value data frame) and `year_counts`
#'   (rows per year with completeness flags from [check_year_completeness()]).
#' @export
npc_archive_info <- function(dir = npc_archive_dir()) {
  mf <- file.path(dir, "manifest.csv")
  yc <- file.path(dir, "year-counts.csv")
  if (!file.exists(mf) || !file.exists(yc))
    stop("No archive found in ", dir, ". Build one with npc_build_archive().",
         call. = FALSE)
  counts <- utils::read.csv(yc)
  list(manifest = utils::read.csv(mf, stringsAsFactors = FALSE),
       year_counts = year_completeness(counts$YEAR, counts$N))
}


#' Read organization-years for a set of years
#'
#' Returns organizations in the canonical schema (see [npc_standardize()])
#' for the requested years, with `CBSA_FIPS` attached.
#'
#' @param years Integer vector of tax years.
#' @param universe Which organizations: `"filers"` (Form 990 and 990-EZ
#'   filers). The registered and grassroots universes are planned.
#' @param source `"archive"` reads the local Parquet archive. Downloading from
#'   NCCS (`"nccs"`) is planned.
#' @param dir Archive directory; see [npc_archive_dir()].
#' @param subsection 501(c) subsections to keep, or `NULL` for all.
#' @param include_imputed Keep rows that fill gap years between filings.
#'   They have no financial data but mark years when the organization
#'   existed.
#' @param cbsa How to assign CBSAs: `"crosswalk"` (default) maps each county
#'   to its CBSA under the 2023 delineation ([cbsa_county_xwalk]), matching
#'   the population data; `"geocode"` uses the CBSA assigned when the
#'   address was geocoded.
#' @param columns Optional character vector of columns to return.
#'
#' @return A data frame of organization-years.
#' @examples
#' \dontrun{
#' orgs <- npc_get_orgs( 2018:2020 )
#' }
#' @export
npc_get_orgs <- function(years,
                         universe = c("filers", "registered", "grassroots"),
                         source = c("archive", "nccs"),
                         dir = npc_archive_dir(),
                         subsection = 3,
                         include_imputed = TRUE,
                         cbsa = c("crosswalk", "geocode"),
                         columns = NULL) {

  universe <- match.arg(universe)
  source   <- match.arg(source)
  cbsa     <- match.arg(cbsa)
  check_planned(universe, source)
  need_pkg("arrow", "read the Parquet archive")

  info <- npc_archive_info(dir)
  years <- as.integer(years)
  absent <- setdiff(years, info$year_counts$YEAR)
  if (length(absent) > 0)
    stop("Year(s) not in the archive: ", paste(absent, collapse = ", "),
         ". Available: ", paste(range(info$year_counts$YEAR), collapse = "-"),
         ".", call. = FALSE)

  ds <- arrow::open_dataset(file.path(dir, "filers"))
  q <- dplyr::filter(ds, YEAR %in% years)
  if (!is.null(subsection)) {
    subs <- as.integer(subsection)
    q <- dplyr::filter(q, SUBSECTION %in% subs)
  }
  if (!include_imputed) q <- dplyr::filter(q, !IMPUTED)
  dat <- as.data.frame(dplyr::collect(q))
  dat$YEAR <- as.integer(dat$YEAR)

  xw <- npcompete::cbsa_county_xwalk
  dat$CBSA_FIPS <- if (cbsa == "crosswalk") {
    xw$CBSA_FIPS[match(dat$COUNTY_FIPS, xw$COUNTY_FIPS)]
  } else {
    dat$CBSA_FIPS_GEOCODE
  }

  cols <- c(setdiff(canonical_cols, "CBSA_FIPS_GEOCODE")[1:3], "CBSA_FIPS",
            setdiff(canonical_cols, c("EIN2", "YEAR", "COUNTY_FIPS")))
  dat <- dat[cols]
  dat <- dat[order(dat$EIN2, dat$YEAR), ]
  rownames(dat) <- NULL
  if (!is.null(columns)) dat <- dat[intersect(columns, names(dat))]
  dat
}


#' Market populations for a geography level
#'
#' @param geo `"county"` or `"cbsa"`.
#' @param years Optional years to keep.
#' @return A data frame with `GEO`, `YEAR` and `POP`, ready to pass as
#'   `pop_table` to [get_scale_free_density()] or [get_all_metrics()].
#' @examples
#' head( npc_get_population( "cbsa", 2020 ) )
#' @export
npc_get_population <- function(geo = c("county", "cbsa"), years = NULL) {
  geo <- match.arg(geo)
  p <- if (geo == "county") {
    data.frame(GEO = npcompete::pop_county$COUNTY_FIPS,
               YEAR = npcompete::pop_county$YEAR,
               POP = npcompete::pop_county$POP)
  } else {
    data.frame(GEO = npcompete::pop_cbsa$CBSA_FIPS,
               YEAR = npcompete::pop_cbsa$YEAR,
               POP = npcompete::pop_cbsa$POP)
  }
  if (!is.null(years)) p <- p[p$YEAR %in% years, ]
  rownames(p) <- NULL
  p
}


# Stop with a clear message if a suggested package is missing.
need_pkg <- function(pkg, why) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Package '", pkg, "' is needed to ", why, ". Install it with ",
         "install.packages(\"", pkg, "\").", call. = FALSE)
}

# Universes and sources that are planned but not yet built.
check_planned <- function(universe, source) {
  if (source == "nccs")
    stop("source = \"nccs\" (live NCCS downloads) is not available yet; ",
         "it is planned for Phase 4 of DEV-PLAN.md. Use source = \"archive\".",
         call. = FALSE)
  if (universe != "filers")
    stop("universe = \"", universe, "\" is not available yet; the registered ",
         "and grassroots universes are planned for Phase 5 of DEV-PLAN.md.",
         call. = FALSE)
}

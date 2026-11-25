###---------------------------------------------------
###   Published metric tables (GitHub release assets)
###---------------------------------------------------

# Where releases live. Tests point these options at a local folder.
release_repo <- function() {
  getOption("npcompete.release_repo", "Nonprofit-Open-Data-Collective/npcompete")
}
release_download_url <- function() {
  getOption("npcompete.release_url",
            paste0("https://github.com/", release_repo(), "/releases/download"))
}


#' List the published versions of the metric tables
#'
#' Precomputed metric tables are published as assets of GitHub releases
#' tagged `metrics-YYYY.MM`. This lists them, newest first.
#'
#' @return A data frame with `vintage` (the release tag), `published` (date)
#'   and `url` (the release page).
#' @seealso [npc_load_metrics()]
#' @examples
#' \dontrun{
#' npc_metric_vintages()
#' }
#' @export
npc_metric_vintages <- function() {
  need_pkg("jsonlite", "read the list of releases from GitHub")
  url <- paste0("https://api.github.com/repos/", release_repo(), "/releases?per_page=100")
  rel <- tryCatch(jsonlite::fromJSON(url, simplifyVector = TRUE),
                  error = function(e)
                    stop("Could not list releases at ", url, ": ", conditionMessage(e),
                         call. = FALSE))
  if (length(rel) == 0 || !"tag_name" %in% names(rel))
    return(data.frame(vintage = character(), published = as.Date(character()),
                      url = character()))
  out <- data.frame(vintage = rel$tag_name,
                    published = as.Date(substr(rel$published_at, 1, 10)),
                    url = rel$html_url, stringsAsFactors = FALSE)
  out <- out[grepl("^metrics-", out$vintage), ]
  out <- out[order(out$published, out$vintage, decreasing = TRUE), ]
  rownames(out) <- NULL
  out
}


#' Load published metric tables
#'
#' Downloads a precomputed metric table from the package's GitHub releases
#' and caches it, so national metrics are available without building them.
#' The tables are built by `data-raw/publish-metrics.R` from public NCCS data
#' with [npc_build_metrics()], for every metric in [npc_metrics()]
#' (dynamics included), 501(c)(3) organizations, CBSAs and counties. Each
#' release's `README.md` gives the years and caveats.
#'
#' @param universe `"filers"`, `"registered"` or `"grassroots"`.
#' @param geo `"cbsa"` or `"county"`.
#' @param resource For filers: `"TOTREV"`, `"TOTEXP"` or `"NETASS"`. The
#'   other universes have no resource.
#' @param vintage Release tag such as `"metrics-2026.11"`, or `"latest"`;
#'   see [npc_metric_vintages()].
#' @param format `"wide"` (one row per market, subsector and year, as
#'   `npc_build_metrics(format = "wide")`) or `"long"` (as its default
#'   output).
#' @param dir Cache directory; see [npc_cache_dir()]. Tables are kept under
#'   `dir/published/<vintage>/`.
#' @param refresh Download again even if cached.
#'
#' @return A tibble, with the build manifest as the `"manifest"` attribute
#'   and the vintage as `"vintage"`. Reads Parquet when the arrow package is
#'   installed, and gzipped CSV otherwise.
#' @seealso [npc_metric_vintages()], [npc_dashboard()]
#' @examples
#' \dontrun{
#' hhi <- npc_load_metrics( "filers", "cbsa", "TOTREV" )
#' subset( hhi, GEO == "35620" & SUB == "ALL", c( YEAR, N, HHI, ENTRY_RATE ) )
#'
#' # Everything in the dashboard
#' npc_dashboard( rbind(
#'   npc_load_metrics( "filers", "cbsa", format = "long" ),
#'   npc_load_metrics( "registered", "cbsa", format = "long" ) ) )
#' }
#' @export
npc_load_metrics <- function(universe = c("filers", "registered", "grassroots"),
                             geo = c("cbsa", "county"),
                             resource = c("TOTREV", "TOTEXP", "NETASS"),
                             vintage = "latest", format = c("wide", "long"),
                             dir = npc_cache_dir(), refresh = FALSE) {
  universe <- match.arg(universe)
  geo      <- match.arg(geo)
  resource <- match.arg(resource)
  format   <- match.arg(format)
  if (identical(vintage, "latest")) {
    v <- npc_metric_vintages()
    if (nrow(v) == 0) stop("No published metric tables found.", call. = FALSE)
    vintage <- v$vintage[1]
  }

  name <- paste(c("metrics", universe, geo, if (universe == "filers") resource),
                collapse = "_")
  parquet <- requireNamespace("arrow", quietly = TRUE)
  file <- paste0(name, if (parquet) ".parquet" else ".csv.gz")
  path <- release_file(vintage, file, dir, refresh)
  mf <- tryCatch(utils::read.csv(release_file(vintage, "manifest.csv", dir, refresh),
                                 stringsAsFactors = FALSE),
                 error = function(e) NULL)

  t <- if (parquet) {
    as.data.frame(arrow::read_parquet(path))
  } else {
    need_pkg("data.table", "read the gzipped CSV table")
    as.data.frame(data.table::fread(path, colClasses = list(character = "GEO"),
                                    showProgress = FALSE))
  }
  t$GEO <- as.character(t$GEO)
  if (!is.null(mf) && "table" %in% names(mf))
    mf <- mf[mf$table == paste(universe, geo, sep = "_"), setdiff(names(mf), "table")]

  out <- if (format == "wide") {
    dplyr::as_tibble(t)
  } else {
    tabs <- list(t)
    by_res <- universe == "filers"
    names(tabs) <- if (by_res) paste(geo, resource, sep = "_") else geo
    metrics_to_long(tabs, universe, by_res)
  }
  attr(out, "manifest") <- mf
  attr(out, "vintage") <- vintage
  out
}

# Download one release asset into the cache, once.
release_file <- function(vintage, file, dir, refresh = FALSE) {
  dest <- file.path(dir, "published", vintage, file)
  if (file.exists(dest) && !refresh) return(dest)
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  url <- paste(release_download_url(), vintage, file, sep = "/")
  tmp <- paste0(dest, ".part")
  old <- options(timeout = max(3600, getOption("timeout")))
  on.exit({ options(old); unlink(tmp) }, add = TRUE)
  ok <- tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !file.exists(tmp))
    stop("Could not download ", url, ". Check the vintage with npc_metric_vintages().",
         call. = FALSE)
  file.rename(tmp, dest)
  dest
}

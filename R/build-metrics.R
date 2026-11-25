###---------------------------------------------------
###   npc_build_metrics(): the front door
###---------------------------------------------------

#' Build competition and density metrics for any set of years
#'
#' Reads organizations for the requested years, attaches market populations,
#' and computes every metric from [get_all_metrics()] for each geography
#' level and resource. Returns a tidy long table (or the familiar wide
#' tables) and can write the results with a manifest of what was used.
#'
#' @param years Integer vector of tax years, for example `2012:2022`.
#' @param geo Geography levels: `"cbsa"`, `"county"`, or both.
#' @param resources Resource columns for the concentration metrics, any of
#'   `"TOTREV"`, `"TOTEXP"`, `"NETASS"`, `"TOTASS"`.
#' @param drop_incomplete_years Drop requested years that hold a partial
#'   panel, judged against the year counts of the whole archive (see
#'   [npc_archive_info()]).
#' @param format `"long"` (one row per market, metric and resource) or
#'   `"wide"` (a named list of tables, one per geography and resource, in the
#'   layout of [get_all_metrics()]).
#' @param out_dir Optional directory. When set, results are written there as
#'   CSV along with `manifest.csv`.
#' @inheritParams npc_get_orgs
#' @inheritParams get_all_metrics
#'
#' @return For `format = "long"`, a tibble with `GEO_LEVEL`, `GEO`, `SUB`,
#'   `YEAR`, `UNIVERSE`, `RESOURCE`, `METRIC` and `VALUE`. For
#'   `format = "wide"`, a named list of tibbles such as `cbsa_TOTREV`. Both
#'   carry a `"manifest"` attribute describing the build.
#'
#'   Rows with `GEO = "ALL"` are national: they include organizations outside
#'   any market of that level (for example, outside every CBSA), and their
#'   `POP` is the U.S. population from [pop_county].
#'
#' @examples
#' \dontrun{
#' options( npcompete.archive_dir = "D:/data/npcompete-archive" )
#'
#' m <- npc_build_metrics( 2015:2020, geo = "cbsa", resources = "TOTREV" )
#' subset( m, GEO == "35620" & SUB == "ALL" & METRIC == "HHI" )
#'
#' # Same tables as the original build script, written to disk
#' npc_build_metrics( 2011:2022, format = "wide", out_dir = "metrics" )
#' }
#' @export
npc_build_metrics <- function(years,
                              geo = c("cbsa", "county"),
                              resources = c("TOTREV", "TOTEXP", "NETASS"),
                              universe = c("filers", "registered", "grassroots"),
                              source = c("archive", "nccs"),
                              dir = npc_archive_dir(),
                              subsection = 3,
                              include_imputed = TRUE,
                              cbsa = c("crosswalk", "geocode"),
                              density_model = c("negbin", "poisson", "loglinear"),
                              cr_n = 1:8,
                              drop_incomplete_years = TRUE,
                              format = c("long", "wide"),
                              out_dir = NULL) {

  universe <- match.arg(universe)
  source   <- match.arg(source)
  cbsa     <- match.arg(cbsa)
  format   <- match.arg(format)
  density_model <- match.arg(density_model)
  geo <- match.arg(geo, c("cbsa", "county"), several.ok = TRUE)
  resources <- match.arg(resources, c("TOTREV", "TOTEXP", "NETASS", "TOTASS"),
                         several.ok = TRUE)
  check_planned(universe, source)

  # --- Years ---------------------------------------------------------------
  years <- sort(unique(as.integer(years)))
  info <- npc_archive_info(dir)
  if (drop_incomplete_years) {
    bad <- intersect(years, info$year_counts$YEAR[info$year_counts$INCOMPLETE])
    if (length(bad) > 0) {
      message("Dropping incomplete years: ", paste(bad, collapse = ", "),
              " (set drop_incomplete_years = FALSE to keep them).")
      years <- setdiff(years, bad)
    }
  }
  if (length(years) == 0) stop("No complete years left to build.", call. = FALSE)

  # --- Data ----------------------------------------------------------------
  message("Reading ", length(years), " year(s) from the archive ...")
  orgs <- npc_get_orgs(years, universe, source, dir, subsection,
                       include_imputed, cbsa)
  us_pop <- stats::aggregate(POP ~ YEAR, data = npcompete::pop_county, FUN = sum)

  geo_col <- c(cbsa = "CBSA_FIPS", county = "COUNTY_FIPS")

  # --- Metrics for each geography x resource -------------------------------
  tables <- list()
  for (g in geo) {
    pop <- npc_get_population(g, years)
    for (r in resources) {
      message("Building ", g, " metrics for ", r, " ...")
      m <- suppressMessages(get_all_metrics(
        orgs, geography = geo_col[[g]], population = NULL, subsector = "NTEEV2",
        resource = r, year = "YEAR", cr_n = cr_n, pop_table = pop,
        density_model = density_model, drop_incomplete_years = FALSE))
      nat <- m$GEO == "ALL"
      m$POP[nat] <- us_pop$POP[match(m$YEAR[nat], us_pop$YEAR)]
      m$LOG_POP[nat] <- log(m$POP[nat] + 1)
      tables[[paste(g, r, sep = "_")]] <- m
    }
  }

  manifest <- c(
    info$manifest$value[match(c("source_file", "built"), info$manifest$key)],
    universe = universe, source = source,
    years = paste(years, collapse = ","),
    geo = paste(geo, collapse = ","), resources = paste(resources, collapse = ","),
    subsection = paste(subsection, collapse = ","),
    include_imputed = include_imputed, cbsa = cbsa,
    cbsa_delineation = 2023, density_model = density_model,
    npcompete_version = as.character(utils::packageVersion("npcompete")),
    built = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  names(manifest)[1:2] <- c("archive_source_file", "archive_built")
  manifest <- data.frame(key = names(manifest), value = unname(manifest))

  out <- if (format == "wide") tables else metrics_to_long(tables, universe)
  attr(out, "manifest") <- manifest

  if (!is.null(out_dir)) write_metrics(out, format, manifest, out_dir)
  out
}


# Stack wide metric tables into one long table.
metrics_to_long <- function(tables, universe) {
  parts <- lapply(names(tables), function(nm) {
    t <- tables[[nm]]
    g <- sub("_.*$", "", nm)
    r <- sub("^[^_]*_", "", nm)
    names(t)[names(t) == r] <- "RESOURCE_TOTAL"
    names(t)[names(t) == paste0("LOG_", r)] <- "LOG_RESOURCE_TOTAL"
    t$GEO_LEVEL <- g
    t$RESOURCE <- r
    tidyr::pivot_longer(t, -c("GEO_LEVEL", "GEO", "SUB", "YEAR", "RESOURCE"),
                        names_to = "METRIC", values_to = "VALUE")
  })
  out <- dplyr::bind_rows(parts)
  out$UNIVERSE <- universe
  dplyr::select(out, "GEO_LEVEL", "GEO", "SUB", "YEAR", "UNIVERSE",
                "RESOURCE", "METRIC", "VALUE")
}

# Write results and manifest to out_dir.
write_metrics <- function(out, format, manifest, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  if (format == "wide") {
    for (nm in names(out)) {
      g <- toupper(sub("_.*$", "", nm))
      r <- sub("^[^_]*_", "", nm)
      f <- file.path(out_dir, paste0("CONCENTRATION-METRICS-BY-", g, "-", r, ".CSV"))
      utils::write.csv(out[[nm]], f, row.names = FALSE, na = "")
    }
  } else {
    utils::write.csv(out, file.path(out_dir, "npcompete-metrics-long.csv"),
                     row.names = FALSE, na = "")
  }
  utils::write.csv(manifest, file.path(out_dir, "manifest.csv"), row.names = FALSE)
  message("Wrote results to ", out_dir)
  invisible(out_dir)
}

###---------------------------------------------------
###   npc_build_metrics(): the front door
###---------------------------------------------------

#' Build competition and density metrics for any set of years
#'
#' Reads organizations for the requested years, attaches market populations,
#' and computes the requested metrics (see [npc_metrics()]) for each
#' geography level and resource. Returns a tidy long table (or wide tables)
#' and can write the results with a manifest of what was used.
#'
#' @param years Integer vector of tax years, for example `2012:2022`.
#' @param geo Geography levels: `"cbsa"`, `"county"`, or both.
#' @param resources Resource columns for the resource-based metrics, any of
#'   `"TOTREV"`, `"TOTEXP"`, `"NETASS"`, `"TOTASS"`.
#' @param metrics Metric ids from [npc_metrics()], or `"default"` for every
#'   metric valid for `universe`.
#' @param window Number of years pooled for each reported year. With
#'   `window = 3`, the value for 2020 uses 2018-2020: each organization's
#'   financial values are averaged over the years it appears, its location
#'   and subsector come from its latest year, and population is the
#'   three-year average. Counts are then organizations active at any point
#'   in the window. Use it to steady metrics for small markets.
#' @param drop_incomplete_years Drop reported years whose data (including
#'   the earlier years of a window) hold a partial panel, judged against the
#'   year counts of the whole archive or live build (see [npc_archive_info()]).
#' @param format `"long"` (one row per market, metric and resource) or
#'   `"wide"` (a named list of tables, one per geography and resource).
#' @param out_dir Optional directory. When set, results are written there as
#'   CSV along with `manifest.csv`.
#' @inheritParams npc_get_orgs
#' @inheritParams npc_compute_metrics
#'
#' @return For `format = "long"`, a tibble with `GEO_LEVEL`, `GEO`, `SUB`,
#'   `YEAR`, `UNIVERSE`, `RESOURCE`, `METRIC` and `VALUE`. Metrics that do not
#'   depend on the resource (counts, age, commercial share, ...) appear once,
#'   with `RESOURCE = NA`. For `format = "wide"`, a named list of tibbles such
#'   as `cbsa_TOTREV` (or `cbsa` when no requested metric depends on the
#'   resource). Both carry a `"manifest"` attribute describing the build.
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
#' # Density and age only, three-year windows, county level
#' npc_build_metrics( 2014:2022, geo = "county", window = 3,
#'                    metrics = c( "DENSITY_PER_100K", "MEDIAN_AGE" ) )
#'
#' # Live build from NCCS downloads (cached under npc_cache_dir())
#' npc_build_metrics( 2019:2021, geo = "cbsa", resources = "TOTREV",
#'                    source = "nccs" )
#' }
#' @export
npc_build_metrics <- function(years,
                              geo = c("cbsa", "county"),
                              resources = c("TOTREV", "TOTEXP", "NETASS"),
                              metrics = "default",
                              universe = c("filers", "registered", "grassroots"),
                              source = c("archive", "nccs"),
                              dir = NULL,
                              subsection = 3,
                              include_imputed = TRUE,
                              cbsa = c("crosswalk", "geocode"),
                              window = 1,
                              density_model = c("negbin", "poisson", "loglinear"),
                              small_max = 1e5, large_min = 1e7,
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
  ids <- resolve_metrics(metrics, universe)
  check_planned(universe, source)
  window <- as.integer(window)
  if (length(window) != 1 || is.na(window) || window < 1)
    stop("`window` must be a whole number of years, 1 or more.", call. = FALSE)

  reg <- metric_registry()
  by_res <- any(reg$by_resource[reg$id %in% ids])
  if (!by_res) resources <- resources[1]

  # --- Years ---------------------------------------------------------------
  years <- sort(unique(as.integer(years)))
  span <- function(t) (t - window + 1):t
  dir <- resolve_data_dir(source, dir)
  if (source == "nccs") {
    need <- unique(unlist(lapply(years, span)))
    ensure_nccs_years(need[need >= 2010], dir)
  }
  info <- npc_archive_info(dir)
  yc <- info$year_counts
  usable <- vapply(years, function(t) {
    s <- span(t)
    all(s %in% yc$YEAR) && (!drop_incomplete_years || !any(yc$INCOMPLETE[yc$YEAR %in% s]))
  }, logical(1))
  if (any(!usable)) {
    message("Dropping year(s) with ", if (window > 1) "incomplete or missing window years" else "incomplete data",
            ": ", paste(years[!usable], collapse = ", "),
            if (drop_incomplete_years) " (set drop_incomplete_years = FALSE to keep them)." else ".")
    years <- years[usable]
  }
  if (length(years) == 0) stop("No usable years left to build.", call. = FALSE)
  read_years <- sort(unique(unlist(lapply(years, span))))

  # --- Data ----------------------------------------------------------------
  message("Reading ", length(read_years), " year(s) from ", if (source == "nccs") "the NCCS build" else "the archive", " ...")
  orgs <- npc_get_orgs(read_years, universe, source, dir, subsection,
                       include_imputed, cbsa)
  if (window > 1) orgs <- window_collapse(orgs, years, window)
  us_pop <- stats::aggregate(POP ~ YEAR, data = npcompete::pop_county, FUN = sum)
  us_pop <- window_population(us_pop, years, window)

  geo_col <- c(cbsa = "CBSA_FIPS", county = "COUNTY_FIPS")

  # --- Metrics for each geography x resource -------------------------------
  tables <- list()
  for (g in geo) {
    pop <- window_population(npc_get_population(g, read_years), years, window)
    for (r in resources) {
      message("Building ", g, " metrics", if (by_res) paste0(" for ", r), " ...")
      m <- suppressMessages(npc_compute_metrics(
        orgs, geography = geo_col[[g]], subsector = "NTEEV2", year = "YEAR",
        resource = r, pop_table = pop, metrics = ids, universe = universe,
        density_model = density_model, small_max = small_max,
        large_min = large_min))
      nat <- m$GEO == "ALL"
      m$POP[nat] <- us_pop$POP[match(m$YEAR[nat], us_pop$YEAR)]
      if ("DENSITY_PER_100K" %in% names(m))
        m$DENSITY_PER_100K[nat] <- m$N[nat] / m$POP[nat] * 1e5
      for (v in intersect(c("SMALL", "LARGE"), sub("_DENSITY_PER_100K", "", names(m))))
        m[[paste0(v, "_DENSITY_PER_100K")]][nat] <- m[[paste0("N_", v)]][nat] / m$POP[nat] * 1e5
      tables[[if (by_res) paste(g, r, sep = "_") else g]] <- m
    }
  }

  manifest <- c(
    info$manifest$value[match(c("source_file", "built"), info$manifest$key)],
    universe = universe, source = source,
    years = paste(years, collapse = ","), window = window,
    geo = paste(geo, collapse = ","),
    resources = if (by_res) paste(resources, collapse = ",") else "",
    metrics = paste(ids, collapse = ","),
    subsection = paste(subsection, collapse = ","),
    include_imputed = include_imputed, cbsa = cbsa,
    cbsa_delineation = 2023, density_model = density_model,
    small_max = small_max, large_min = large_min,
    npcompete_version = as.character(utils::packageVersion("npcompete")),
    built = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  names(manifest)[1:2] <- c("data_source_file", "data_built")
  manifest <- data.frame(key = names(manifest), value = unname(manifest))

  out <- if (format == "wide") tables else metrics_to_long(tables, universe, by_res)
  attr(out, "manifest") <- manifest

  if (!is.null(out_dir)) write_metrics(out, format, manifest, out_dir)
  out
}


# Pool `window` years for each reported year: one row per organization and
# reported year, with financial values averaged over the years present and
# location, subsector and other attributes from the latest year.
window_collapse <- function(orgs, years, window) {
  fin <- intersect(c("TOTREV", "TOTEXP", "NETASS", "TOTASS", "TOTLIA",
                     "REVPROG", "REVCONT", "REVINVE", "FNDFEE"), names(orgs))
  dt <- data.table::as.data.table(orgs)
  parts <- lapply(years, function(t) {
    w <- dt[YEAR %in% ((t - window + 1):t)]
    data.table::setorderv(w, c("EIN2", "YEAR"))
    means <- w[, lapply(.SD, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)),
               by = "EIN2", .SDcols = fin]
    latest <- w[, .SD[.N], by = "EIN2", .SDcols = setdiff(names(w), c("EIN2", fin))]
    res <- merge(latest, means, by = "EIN2")
    res$YEAR <- as.integer(t)
    res
  })
  out <- as.data.frame(data.table::rbindlist(parts, use.names = TRUE))
  out[names(orgs)]
}

# Average population over each reported year's window.
window_population <- function(pop, years, window) {
  if (window == 1) return(pop[pop$YEAR %in% years, ])
  geo_col <- intersect(c("GEO"), names(pop))
  parts <- lapply(years, function(t) {
    p <- pop[pop$YEAR %in% ((t - window + 1):t), ]
    a <- if (length(geo_col)) stats::aggregate(POP ~ GEO, data = p, FUN = mean)
         else data.frame(POP = mean(p$POP))
    a$YEAR <- t
    a
  })
  out <- do.call(rbind, parts)
  out[c(geo_col, "YEAR", "POP")]
}


# Stack wide metric tables into one long table.
metrics_to_long <- function(tables, universe, by_res = TRUE) {
  reg <- metric_registry()
  indep <- c("N", "POP", "N_AGE", "N_COMM", "N_FEE",
             reg$id[!reg$by_resource])
  parts <- lapply(seq_along(tables), function(i) {
    nm <- names(tables)[i]
    t <- tables[[nm]]
    g <- sub("_.*$", "", nm)
    r <- if (by_res) sub("^[^_]*_", "", nm) else NA_character_
    if (!is.na(r)) names(t)[names(t) == r] <- "RESOURCE_TOTAL"
    t$GEO_LEVEL <- g
    long <- tidyr::pivot_longer(t, -c("GEO_LEVEL", "GEO", "SUB", "YEAR"),
                                names_to = "METRIC", values_to = "VALUE")
    long$RESOURCE <- ifelse(long$METRIC %in% indep, NA_character_, r)
    first_for_geo <- i == match(g, sub("_.*$", "", names(tables)))
    if (!first_for_geo) long <- long[!is.na(long$RESOURCE), ]   # keep one copy
    long
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
      f <- file.path(out_dir, paste0("NPCOMPETE-METRICS-BY-",
                                     toupper(gsub("_", "-", nm)), ".CSV"))
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

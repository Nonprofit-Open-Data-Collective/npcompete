###---------------------------------------------------
###   Metric registry and registry-driven computation
###---------------------------------------------------

all_universes <- c("filers", "registered", "grassroots")

# One row per metric. `family` decides which function computes it;
# `financial` metrics need Form 990/990-EZ financial data and so only work
# for filers; `by_resource` metrics depend on the chosen resource column.
metric_registry <- function() {
  r <- function(id, family, financial, by_resource, description,
                universes = if (financial) "filers" else all_universes,
                default = TRUE) {
    data.frame(id = id, family = family, financial = financial,
               by_resource = by_resource,
               universes = paste(universes, collapse = ","),
               default = default,
               description = description, stringsAsFactors = FALSE)
  }
  rbind(
    r("DENSITY_PER_100K", "count", FALSE, FALSE,
      "Organizations per 100,000 residents"),
    r("DENSITY_PCTL_N", "scale", FALSE, FALSE,
      "Organization count relative to the count expected for the market's population"),
    r("MEDIAN_AGE", "age", FALSE, FALSE,
      "Median years since the IRS ruling year"),
    r("GRASSROOTS_SHARE", "grassroots", FALSE, FALSE,
      "Share of registered organizations that file a 990-N or do not file",
      universes = "registered"),
    r("SMALL_DENSITY_PER_100K", "size", TRUE, TRUE,
      "Small organizations (resource below small_max) per 100,000 residents"),
    r("LARGE_DENSITY_PER_100K", "size", TRUE, TRUE,
      "Large organizations (resource at or above large_min) per 100,000 residents"),
    r("SMALL_SHARE", "size", TRUE, TRUE, "Share of organizations that are small"),
    r("LARGE_SHARE", "size", TRUE, TRUE, "Share of organizations that are large"),
    r("DENSITY_PCTL_R", "scale", TRUE, TRUE,
      "Resource total relative to the total expected for the market's population"),
    r("HHI", "hhi", TRUE, TRUE, "Herfindahl-Hirschman Index of resource shares"),
    r("NHHI", "hhi", TRUE, TRUE, "HHI normalized to 0-1 for the number of organizations"),
    do.call(rbind, lapply(1:8, function(k)
      r(paste0("CR", k), "cr", TRUE, TRUE,
        sprintf("Share of the resource held by the %d largest organization%s",
                k, if (k > 1) "s" else "")))),
    r("KWOKA", "kwoka", TRUE, TRUE, "Kwoka dominance index"),
    r("GINI", "gini", TRUE, TRUE, "Gini coefficient of the resource"),
    r("COMMERCIAL_SHARE", "commercial", TRUE, FALSE,
      "Share of organizations with program revenue above half of total revenue"),
    r("PROGRAM_REVENUE_SHARE", "commercial", TRUE, FALSE,
      "Program service revenue as a share of total revenue"),
    r("FUNDRAISER_SHARE", "fundraising", TRUE, FALSE,
      "Share of Form 990 filers paying professional fundraisers"),
    r("FUNDRAISING_RATIO", "fundraising", TRUE, FALSE,
      "Contributions per dollar of professional fundraising fees"),
    r("ENTRY_RATE", "dynamics", FALSE, FALSE,
      "Organizations entering in t per organization in t - 1", default = FALSE),
    r("EXIT_RATE", "dynamics", FALSE, FALSE,
      "Organizations exiting after t - 1 per organization in t - 1", default = FALSE),
    r("NET_GROWTH", "dynamics", FALSE, FALSE,
      "Change in the number of organizations since t - 1, as a share", default = FALSE),
    r("CHURN_RATE", "dynamics", FALSE, FALSE,
      "Entries plus exits per organization in t - 1", default = FALSE),
    r("GRADUATION_RATE", "dynamics", FALSE, FALSE,
      "Share of 990-N filers and non-filers in t - 1 that file a 990 or 990-EZ in t",
      universes = "registered", default = FALSE),
    r("DOWNGRADE_RATE", "dynamics", FALSE, FALSE,
      "Share of 990 and 990-EZ filers in t - 1 that file a 990-N or do not file in t",
      universes = "registered", default = FALSE)
  )
}


#' List the available metrics
#'
#' The metric registry: every metric [npc_compute_metrics()] and
#' [npc_build_metrics()] can produce, which universes of organizations it is
#' valid for, and whether it needs financial data.
#'
#' @param universe Optional universe (`"filers"`, `"registered"` or
#'   `"grassroots"`) to list only the metrics valid for it.
#'
#' @return A data frame with `id`, `family`, `financial` (needs Form 990 or
#'   990-EZ financial data), `by_resource` (depends on the resource column),
#'   `default` (included in `metrics = "default"`; dynamics metrics are not,
#'   since they read extra years),
#'   `universes` and `description`.
#'
#' @details Financial metrics are limited to filers: organizations that file
#'   the Form 990-N postcard or file nothing do not report revenue, expenses
#'   or assets.
#'
#' @examples
#' npc_metrics()
#' npc_metrics( "grassroots" )
#'
#' @export
npc_metrics <- function(universe = NULL) {
  reg <- metric_registry()
  if (!is.null(universe)) {
    universe <- match.arg(universe, all_universes)
    reg <- reg[grepl(universe, reg$universes), ]
  }
  rownames(reg) <- NULL
  reg
}


# Validate requested metric ids against the registry and the universe.
resolve_metrics <- function(metrics, universe = "filers") {
  universe <- match.arg(universe, all_universes)
  reg <- metric_registry()
  if (identical(metrics, "all")) {
    return(reg$id[grepl(universe, reg$universes)])
  }
  if (identical(metrics, "default")) {
    reg <- reg[reg$default, ]
    return(reg$id[grepl(universe, reg$universes)])
  }
  unknown <- setdiff(metrics, reg$id)
  if (length(unknown) > 0)
    stop("Unknown metric(s): ", paste(unknown, collapse = ", "),
         ". See npc_metrics() for the list.", call. = FALSE)
  bad <- reg[reg$id %in% metrics & !grepl(universe, reg$universes), ]
  fin <- bad[bad$financial, ]
  if (nrow(fin) > 0)
    stop(paste(fin$id, collapse = ", "),
         if (nrow(fin) == 1) " requires" else " require",
         " financial data; ", universe, " organizations do not report ",
         "revenue, expenses or assets. Use universe = \"filers\", or see ",
         "npc_metrics(\"", universe, "\") for metrics valid for ", universe, ".",
         call. = FALSE)
  if (nrow(bad) > 0)
    stop(paste(bad$id, collapse = ", "), " ", if (nrow(bad) == 1) "is" else "are",
         " defined only for universe = \"", bad$universes[1], "\".", call. = FALSE)
  metrics
}


#' Compute a set of metrics from the registry
#'
#' Runs only the metric families needed for the requested metrics and joins
#' them onto a market frame that has every market in `pop_table`, with zero
#' counts where a market has no organizations.
#'
#' @inheritParams npcompete-params
#' @inheritParams get_density_per_capita
#' @inheritParams get_scale_free_density
#' @param metrics Metric ids from [npc_metrics()], or `"default"` for every
#'   metric valid for `universe`.
#' @param universe Universe the data represent; used to check that the
#'   requested metrics are valid for it.
#' @param cr_n Not used; concentration ratios are chosen through `metrics`
#'   (`"CR1"` to `"CR8"`). Kept for compatibility with [get_all_metrics()].
#' @param density_model Model for `DENSITY_PCTL_N` and `DENSITY_PCTL_R`;
#'   passed to [get_scale_free_density()] as `model`.
#' @param ruling,program,contributions,fees Columns for the ruling year,
#'   program service revenue, contributions and professional fundraising
#'   fees.
#' @param years Years to report. `df` may hold more years, which only the
#'   dynamics metrics use (to look back to `t - 1` and ahead to confirm
#'   exits). Defaults to every year in `df`.
#' @param id,filer_type Organization id and filer type columns, for the
#'   dynamics metrics.
#' @inheritParams get_dynamics
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `N` (all organizations), `POP`
#'   and the requested metrics. Supporting counts are kept where relevant:
#'   `N_RES` and the resource total for resource metrics, `N_SMALL` /
#'   `N_LARGE`, `N_AGE`, `N_COMM` and `N_FEE`.
#'
#' @examples
#' m <- npc_compute_metrics( npo_panel_sample, geography = "COUNTY_FIPS",
#'                           year = "TAXYEAR", ruling = "ORG_RULING_YEAR",
#'                           pop_table = npc_get_population( "county", 2015:2019 ),
#'                           metrics = c( "DENSITY_PER_100K", "HHI", "MEDIAN_AGE" ) )
#' head( m[ m$SUB == "ALL", ] )
#'
#' @export
npc_compute_metrics <- function(df, geography = "CBSA_FIPS",
                                subsector = "NTEEV2", year = "YEAR",
                                resource = "TOTREV", pop_table,
                                metrics = "default", universe = "filers",
                                cr_n = 1:8,
                                density_model = c("negbin", "poisson", "loglinear"),
                                small_max = 1e5, large_min = 1e7,
                                ruling = "RULING_YEAR", program = "REVPROG",
                                contributions = "REVCONT", fees = "FNDFEE",
                                years = NULL, id = "EIN2", filer_type = "FILER_TYPE",
                                absent_years = 2, observed_years = NULL,
                                drop_ppa_revocations = FALSE) {

  density_model <- match.arg(density_model)
  ids <- resolve_metrics(metrics, universe)
  df_all <- df
  if (!is.null(years)) df <- df[df[[year]] %in% years, , drop = FALSE]
  reg <- metric_registry()
  fam <- unique(reg$family[reg$id %in% ids])
  cr_k <- as.integer(sub("CR", "", intersect(paste0("CR", 1:8), ids)))
  needs_res <- any(reg$by_resource[reg$id %in% ids])

  key <- c("GEO", "SUB", "YEAR")
  keep <- function(t, cols) t[, intersect(c(key, cols), names(t)), drop = FALSE]

  # Base frame: counts on every market, plus size classes if requested
  base <- get_density_per_capita(df, geography, subsector, year, pop_table,
                                 resource = if (needs_res) resource else NULL,
                                 small_max = small_max, large_min = large_min)
  base_cols <- c("POP", "N", if (needs_res) "N_RES",
                 if ("size" %in% fam) c("N_SMALL", "N_LARGE"),
                 intersect(ids, names(base)))
  out <- keep(base, base_cols)
  out$YEAR <- as_year(out$YEAR, df[[year]])

  add <- function(t, cols) {
    t <- keep(t, cols)
    t$YEAR <- as_year(t$YEAR, df[[year]])
    dplyr::left_join(out, t, by = key)
  }

  if (needs_res)                       # resource total, plus HHI / NHHI if requested
    out <- add(get_nhhi(df, geography, subsector, resource, year),
               c(resource, intersect(c("HHI", "NHHI"), ids)))
  if ("cr" %in% fam)
    out <- add(get_cr_all(df, geography, subsector, resource, year, numbers = cr_k), paste0("CR", cr_k))
  if ("kwoka" %in% fam)
    out <- add(get_kwoka(df, geography, subsector, resource, year), "KWOKA")
  if ("gini" %in% fam)
    out <- add(get_gini(df, geography, subsector, resource, year), "GINI")
  if ("scale" %in% fam) {
    sf <- get_scale_free_density(df, geography, NULL, subsector, year,
                                 resource = resource, pop_table = pop_table,
                                 model = density_model)
    out <- add(sf, intersect(c("DENSITY_PCTL_N", "DENSITY_PCTL_R"), ids))
  }
  if ("age" %in% fam)
    out <- add(get_median_age(df, geography, subsector, year, ruling), c("N_AGE", "MEDIAN_AGE"))
  if ("dynamics" %in% fam) {
    trans <- any(c("GRADUATION_RATE", "DOWNGRADE_RATE") %in% ids)
    dyn <- get_dynamics(df_all, geography, subsector, year, id = id,
                        filer_type = if (trans) filer_type,
                        absent_years = absent_years, observed_years = observed_years,
                        drop_ppa_revocations = drop_ppa_revocations)
    out <- add(dyn, c(dynamics_support(ids), intersect(ids, names(dyn))))
  }
  if ("grassroots" %in% fam)
    out <- add(get_grassroots_share(df, geography, subsector, year),
               c("N_GRASSROOTS", "GRASSROOTS_SHARE"))
  if ("commercial" %in% fam)
    out <- add(get_commercial_share(df, geography, subsector, year, revenue = "TOTREV",
                                    program = program),
               c("N_COMM", intersect(c("COMMERCIAL_SHARE", "PROGRAM_REVENUE_SHARE"), ids)))
  if ("fundraising" %in% fam)
    out <- add(get_fundraising_ratio(df, geography, subsector, year,
                                     contributions = contributions, fees = fees),
               c("N_FEE", intersect(c("FUNDRAISER_SHARE", "FUNDRAISING_RATIO"), ids)))

  # Supporting counts are 0, not NA, in markets with no organizations
  for (v in intersect(c("N_AGE", "N_COMM", "N_FEE", "N_GRASSROOTS"), names(out)))
    out[[v]][is.na(out[[v]])] <- 0L

  ordered <- c(key, "N", "POP", "N_RES", resource, "N_SMALL", "N_LARGE",
               "N_AGE", "N_GRASSROOTS", "N_COMM", "N_FEE", dynamics_support(ids),
               reg$id[reg$id %in% ids])
  out <- out[, intersect(ordered, names(out))]
  attr(out, "metrics") <- ids
  dplyr::as_tibble(out)
}

# Match a YEAR column's type to the input year column.
as_year <- function(x, ref) {
  if (is.numeric(ref)) as.numeric(as.character(x)) else as.character(x)
}

# Count columns kept alongside the requested dynamics metrics.
dynamics_support <- function(ids) {
  reg <- metric_registry()
  if (!any(reg$family[reg$id %in% ids] == "dynamics")) return(character())
  c("N_PREV", "ENTRIES", "EXITS", "EXITS_PPA",
    if ("GRADUATION_RATE" %in% ids) c("N_PREV_GRASSROOTS", "GRADUATIONS"),
    if ("DOWNGRADE_RATE" %in% ids) c("N_PREV_FILERS", "DOWNGRADES"))
}

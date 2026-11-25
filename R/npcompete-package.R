#' @keywords internal
"_PACKAGE"

#' @importFrom magrittr %>%
#' @importFrom dplyr select rename mutate arrange bind_rows desc
#' @importFrom rlang :=
NULL

# Column names used inside dplyr/tidyr verbs (non-standard evaluation).
utils::globalVariables( c(
  "GEO", "SUB", "YEAR", "RES", "POP", "POP2", "RES2", "N", "LOG_RES",
  "share", "top1", "top2", "top3", "total_top3", "top_k_sum", "cr",
  "sorted", "total", "cr_list", "data", "model_n", "model_r",
  "model_results_n", "model_results_r", "geo", "subsector", "resource",
  "countypop", "geopop", "commercialperc", "resource_total",
  "fundraisingexp", "n_commercial", "county_population", "FIPS", "n",
  "LOG_NPO", "LOG_POP", "DENSITY_PCTL_N", "DENSITY_PCTL_R",
  "EXPECTED_N", "EXPECTED_RES", "SUBSECTION", "IMPUTED", "EIN2", ".N", ".SD",
  "FILER_TYPE", "CHURCH", "NEXT", "PREV_Y", "NEXT_FT", "Y", "FT", "ID",
  "VALUE", "VALUE_2", "SERIES", "NAME"
) )

# Tell data.table that this package uses data.table syntax.
.datatable.aware <- TRUE

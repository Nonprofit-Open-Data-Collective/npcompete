###---------------------------------------------------
###   Concentration Ratio
###---------------------------------------------------

#' Concentration ratio (CRk)
#'
#' Share of a market's total resource held by its `number` largest
#' organizations, computed for each geography, subsector and year.
#'
#' @inheritParams npcompete-params
#' @param number Number of top organizations to include, for example `4` for
#'   CR4. Values above 8 produce a warning.
#'
#' @return A tibble with one row per `GEO`, `SUB` and `YEAR`, containing `n`
#'   (number of organizations), `total`, `top_k_sum` and `CR<number>`. Markets
#'   with `number` or fewer organizations get a ratio of 1.
#'
#' @details CRk is the sum of the resource held by the k largest organizations
#'   divided by the market total.
#'
#' @examples
#' \dontrun{
#' get_cr( df, geography = "CENSUS_CBSA_FIPS", resource = "TOTREV", number = 4 )
#' }
#'
#' @export
get_cr <- function(df, geography="CENSUS_CBSA_FIPS", subsector="NTEEV2", year="TAXYEAR", resource="TOTREV", number = 4){

  if (number > 8) warning("Number > 8; top 8 is typical for CR measures.")

  GEO <- df[[geography]] |> factor( exclude = NULL )
  RES <- df[[resource]]  |> as.numeric() |> suppressWarnings()
  if( subsector == "NTEEV2" )
  { SUB <- df[[subsector]] |> substr(1,3) |> factor( exclude = NULL ) } else
  { SUB <- df[[subsector]] |> factor( exclude = NULL ) }
  YEAR <- df[[year]] |> factor() 
  NUM <- number
    
  df %>%
    data.frame(GEO,SUB,YEAR,RES) %>%
    dplyr::mutate(
      resource = as.numeric(RES),
      resource = ifelse( RES < 0, 0, RES)
    ) %>%
    dplyr::group_by( GEO, SUB, YEAR ) %>%
    dplyr::summarize(
      n = dplyr::n(),
      total = sum( RES, na.rm = TRUE),
      top_k_sum = sum(utils::head(sort(RES, decreasing = TRUE), NUM), na.rm = TRUE),
      cr = top_k_sum / total,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      !!paste0("CR", NUM) := dplyr::if_else(n <= NUM, 1, cr)
    ) %>%
    dplyr::select(-cr)
}


#' Concentration ratios CR1 to CRk
#'
#' Computes several concentration ratios at once for each geography,
#' subsector and year, with rollups for all subsectors (`SUB = "ALL"`) and all
#' geographies (`GEO = "ALL"`).
#'
#' @inheritParams npcompete-params
#' @param numbers Integer vector of CR levels to calculate, for example
#'   `c(1, 4, 8)`.
#' @param as_percent If `TRUE`, ratios are returned on a 0-100 scale instead
#'   of 0-1.
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `N`, the summed resource and one
#'   column per requested ratio (`CR1`, `CR2`, ...).
#'
#' @examples
#' \dontrun{
#' get_cr_all( df, resource = "TOTREV", numbers = 1:8 )
#' }
#'
#' @export
get_cr_all <- function(df,
                       geography = "CENSUS_CBSA_FIPS",
                       subsector = "NTEEV2",
                       year      = "TAXYEAR",
                       resource  = "TOTREV",
                       numbers   = c(1,2,3,4,5,6,7,8),
                       as_percent = FALSE) {

  if (any(numbers > 8))
    warning("Values > 8 uncommon; CR8 is a conventional upper limit.")

  # Build working variables
  GEO  <- factor(df[[geography]], exclude = NULL)
  RES  <- suppressWarnings(as.numeric(df[[resource]]))
  SUB  <- if (identical(subsector, "NTEEV2")) {
    factor(substr(df[[subsector]], 1, 3), exclude = NULL)
  } else {
    factor(df[[subsector]], exclude = NULL)
  }
  YEAR <- factor(df[[year]])

  # Helper function
  cr_from_sorted <- function(sorted, total, n, k_vec, pct = FALSE) {
    vals <- vapply(k_vec, function(k) {
      ratio <- if (n <= k) 1 else sum(utils::head(sorted, k), na.rm = TRUE) / total
      if (pct) 100 * ratio else ratio
    }, numeric(1))
    stats::setNames(vals, paste0("CR", k_vec))
  }

  out1 <- 
    data.frame(GEO, SUB, YEAR, RES) |>
    stats::na.omit() %>%
    dplyr::mutate(RES = pmax(0, as.numeric(RES))) |>
    dplyr::group_by(GEO, SUB, YEAR) |>
    dplyr::summarize(
      N      = dplyr::n(),
      total  = sum(RES, na.rm = TRUE),
      sorted = list(sort(RES, decreasing = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      cr_list = list(
        cr_from_sorted(
          sorted = sorted,
          total  = total,
          n      = N,
          k_vec  = numbers,
          pct    = as_percent
        )
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::unnest_wider(cr_list) |>
    dplyr::select(-sorted) |>
    dplyr::rename( {{resource}} := total )


  out2 <- 
    data.frame(GEO, YEAR, RES) |>
    stats::na.omit() %>%
    dplyr::mutate(RES = pmax(0, as.numeric(RES))) |>
    dplyr::group_by(GEO, YEAR) |>
    dplyr::summarize(
      N      = dplyr::n(),
      total  = sum(RES, na.rm = TRUE),
      sorted = list(sort(RES, decreasing = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      cr_list = list(
        cr_from_sorted(
          sorted = sorted,
          total  = total,
          n      = N,
          k_vec  = numbers,
          pct    = as_percent
        )
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::unnest_wider(cr_list) |>
    dplyr::select(-sorted) |>
    dplyr::rename( {{resource}} := total )
    
  out2$SUB <- "ALL"
  
  out3 <- 
    data.frame(SUB, YEAR, RES) |>
    stats::na.omit() %>%
    dplyr::mutate(RES = pmax(0, as.numeric(RES))) |>
    dplyr::group_by(SUB, YEAR) |>
    dplyr::summarize(
      N      = dplyr::n(),
      total  = sum(RES, na.rm = TRUE),
      sorted = list(sort(RES, decreasing = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      cr_list = list(
        cr_from_sorted(
          sorted = sorted,
          total  = total,
          n      = N,
          k_vec  = numbers,
          pct    = as_percent
        )
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::unnest_wider(cr_list) |>
    dplyr::select(-sorted) |>
    dplyr::rename( {{resource}} := total )

  out3$GEO <- "ALL"
  
  out <- bind_rows( out1, out2, out3 )
  out <- arrange( out, GEO, SUB, YEAR )
    
  return(out)
}

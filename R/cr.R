###---------------------------------------------------
###   Concentration Ratio
###---------------------------------------------------

#' @title
#' Concentration Ratio
#'
#' @description
#' Calculate the Concentration Ratio (CR) of top n(number) largest organizations and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(concentration ratio) grouped by geographical level and sub sector.
#' @param geo A character string indicating the column name for geographical area unique code.
#' @param subsector A character string indicating the column name for sub sectors.
#' @param resource A character string indicating the column name for resource based on which cr is calculated. Example: Revenue, assets etc.
#' @param number A numeric value that is used to calculate concentration ratio for top that number of firms. Value ranges from 1-8. Example, 4 if concentration ratio for top 4 firms.
#'
#' @return A new dataframe with the Concentration Ratio of n(number) largest organizations ('cr'),
#'  grouped by geographical level and sub sectors *Note: For the markets where total number of firms is less than n(umber),
#'  the concentration ratio is assigned 100%.
#'
#' @details Concentration Ratio is defined as the summation of the market shares of the largest n firms.
#'  It is calculated using the summation of revenue of the largest n nonprofits divided by the summation of
#'  revenue of all nonprofits in a market. The formula is \eqn{\frac{\sigma{revnue of top-n firms}}{Total resource of that subsector}}
#'
#' @examples
#' data(nonprofit_sample)
#  dat.cr <-get_cr( df=nonprofit_sample,'MSA_NECH','NTMAJ12','TOTREV',4)
#' head( dat.cr )
#'
#' @export
#' Compute Concentration Ratio (CRk)
#'
#' Calculates the concentration ratio of the top `k` firms (or organizations)
#' within each group, defined as the proportion of total market resources
#' controlled by the largest `k` firms.
#'
#' @param df A data frame containing the grouping and resource variables.
#' @param geo,subsector Unquoted grouping variables (e.g. GEO, SUB).
#' @param resource Unquoted variable indicating firm size (e.g. revenue).
#' @param number Integer. The number of top firms to include (e.g. 4 for CR4).
#'
#' @return A tibble with one row per group and columns:
#'   \item{n}{number of firms}
#'   \item{total}{sum of resources}
#'   \item{CRk}{share of top `k` firms (0–1 scale)}
#'
#' @examples
#' get_cr(df, GEO, SUB, RES, number = 4)
#'
#' @export
get_cr <- function(df, geography="CENSUS_CBSA_FIPS", subsector="NTEEV2", year="TAXYEAR", resource="TOTREV", number = 4){

  if (number > 8) warning("Number > 8 — top 8 is typical for CR measures.")

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


#' Compute Multiple Concentration Ratios (CR1–CRk)
#'
#' Calculates concentration ratios (CR1, CR2, ..., CRk) for each group,
#' where CRk is the proportion of total resources held by the top k firms.
#'
#' @param df A data frame containing the grouping and resource variables.
#' @param geo,subsector Unquoted grouping variables (e.g., GEO, SUB).
#' @param resource Unquoted variable indicating firm size (e.g., revenue).
#' @param numbers Integer vector of CR levels to calculate (e.g., c(1,3,4,5,8)).
#'
#' @return A tibble with one row per group and columns:
#'   \item{n}{Number of firms in the group}
#'   \item{total}{Sum of resource values}
#'   \item{CRk}{Concentration ratio for top k firms (0–1 scale)}
#'
#' @examples
#' get_cr_all(df, GEO, SUB, RES, numbers = c(1,3,4,5,8))
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
    na.omit() %>%
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
    na.omit() %>%
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
    na.omit() %>%
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

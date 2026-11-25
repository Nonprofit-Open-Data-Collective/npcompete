###---------------------------------------------------
###   Kwoka Index
###---------------------------------------------------

#' Kwoka index
#'
#' Concentration among the three largest organizations in each geography,
#' subsector and year, with rollups for all subsectors (`SUB = "ALL"`) and all
#' geographies (`GEO = "ALL"`).
#'
#' @inheritParams npcompete-params
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `N`, the shares of the three
#'   largest organizations (`top1`, `top2`, `top3`, `total_top3`) and `KWOKA`.
#'   Markets with fewer than three organizations get `KWOKA = 1`.
#'
#' @details `KWOKA` is computed as the sum of the squared shares of the top
#'   three organizations, each rescaled by their combined share.
#'
#' @examples
#' toy <- data.frame( GEO = "A", SUB = "ART", YEAR = 2020,
#'                    TOTREV = c( 50, 30, 10, 10 ) )
#' get_kwoka( toy, geography = "GEO", subsector = "SUB", year = "YEAR" )
#'
#' @export
get_kwoka <- function(df, geography = "CENSUS_CBSA_FIPS", subsector = "NTEEV2",
                      resource = "TOTREV", year = "TAXYEAR") {

  dat <- prep_market_data(df, geography, subsector, resource, year)
  summarize_levels(dat, kwoka_kernel)
}

# Per-market Kwoka index from the top three shares.
kwoka_kernel <- function(x) {
  n <- length(x)
  share <- sort(x / sum(x), decreasing = TRUE)
  top <- share[1:3]                     # NA when fewer than three orgs
  total_top3 <- sum(top, na.rm = TRUE)
  k <- sum((top / total_top3)^2, na.rm = TRUE)
  if (n < 3) k <- 1
  data.frame(N = n, top1 = top[1], top2 = top[2], top3 = top[3],
             total_top3 = total_top3, KWOKA = k)
}

#' Compute the Kwoka Index of Market Concentration
#'
#' The Kwoka index measures the inequality of market shares among
#' the top `k` firms (or organizations) in a market or region.
#' It equals 1 when one firm dominates and 1/k when the top k firms
#' have equal shares of their combined total.
#'
#' @param data A data frame containing at least the grouping variables and `share_var`.
#' @param share_var A string giving the name of the variable representing market shares
#'   or raw quantities (e.g., revenues). If raw quantities are provided, they are
#'   automatically normalized within each group.
#' @param group_vars Character vector of grouping variables (e.g., `c("GEO", "SUB", "YEAR")`).
#' @param k Integer specifying how many top firms to include (default = 3).
#'
#' @return A data frame with one observation per group containing:
#' \describe{
#'   \item{n}{Number of firms in the group}
#'   \item{kwoka}{The computed Kwoka index}
#'   \item{top_k_share}{Combined share of the top k firms}
#' }
#'
#' @examples
#' \dontrun{
#' get_kwoka_k( df, "RES", c("GEO", "SUB", "YEAR"), k = 3 )
#' }
#'
#' @export
get_kwoka_k <- function(data, share_var, group_vars, k = 3) {

  share_sym <- rlang::sym(share_var)

  data %>%
    dplyr::filter(!is.na(!!share_sym)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::mutate(share = !!share_sym / sum(!!share_sym, na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(share), .by_group = TRUE) %>%
    dplyr::summarize(
      n = dplyr::n(),
      top_k = list(utils::head(share, k)),
      top_k_share = sum(utils::head(share, k), na.rm = TRUE),
      kwoka = {
        topk <- utils::head(share, k)
        sum((topk / sum(topk, na.rm = TRUE))^2, na.rm = TRUE)
      },
      .groups = "drop"
    )
}

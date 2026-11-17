###---------------------------------------------------
###   Kwoka Index
###---------------------------------------------------

#' @title
#' Kwoka Index
#'
#' @description
#' Calculate the Kwoka Index (kindex) and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(kwoka index) grouped by geographical level and sub sector.
#' @param geo A character string indicating the column name for georaphical area unique code.
#' @param subsector A character string indicating the column name for sub sectors.
#' @param resource A character string indicating the column name for resource based on which cr4 is calculated. Example: Revenue, assets etc.
#'
#' @return A new dataframe with the Kwoka Index (`kindex`),
#'  grouped by geographical level and sub sectors.
#'
#' @details The Kwoka Index(kindex) is a measure of market concentration that is used to understand market competitiveness,
#'  competitive behavior and performance in markets.
#'
#' @examples
#' data(nonprofit_sample)
#  dat.kindex <-get_kwoka_index( df=nonprofit_sample,'MSA_NECH','NTMAJ12','TOTREV')
#' head( dat.kindex )
#'
#' @export
get_kwoka <- function(df, geography="CENSUS_CBSA_FIPS", subsector="NTEEV2", resource="TOTREV", year="TAXYEAR"){

  GEO <- df[[geography]] |> factor( exclude = NULL )
  RES <- df[[resource]]  |> as.numeric() |> suppressWarnings()
  if( subsector == "NTEEV2" )
  { SUB <- df[[subsector]] |> substr(1,3) |> factor( exclude = NULL ) } else
  { SUB <- df[[subsector]] |> factor( exclude = NULL ) }

  RES <- bottomcode(RES)

  YEAR <- df[[year]] |> factor() 

  dat.k1 <- 
    data.frame(GEO,SUB,YEAR,RES) %>%
    na.omit() %>%
    dplyr::filter(!is.na(RES)) %>%
    dplyr::group_by(GEO, SUB, YEAR) %>%
    dplyr::mutate(share = RES / sum(RES, na.rm = TRUE)) %>%  # normalize to shares
    dplyr::arrange(desc(share)) %>%
    dplyr::summarize(
      N = dplyr::n(),
      top1 = dplyr::nth(share, 1),
      top2 = dplyr::nth(share, 2),
      top3 = dplyr::nth(share, 3),
      total_top3 = sum(c(top1, top2, top3), na.rm = TRUE),
      KWOKA = sum((c(top1, top2, top3) / total_top3)^2, na.rm = TRUE),
      .groups = "drop"
    )

  dat.k1$KWOKA[dat.k1$N < 3] <- 1

  dat.k2 <- 
    data.frame(GEO,YEAR,RES) %>%
    na.omit() %>%
    dplyr::filter(!is.na(RES)) %>%
    dplyr::group_by(GEO, YEAR) %>%
    dplyr::mutate(share = RES / sum(RES, na.rm = TRUE)) %>%  # normalize to shares
    dplyr::arrange(desc(share)) %>%
    dplyr::summarize(
      N = dplyr::n(),
      top1 = dplyr::nth(share, 1),
      top2 = dplyr::nth(share, 2),
      top3 = dplyr::nth(share, 3),
      total_top3 = sum(c(top1, top2, top3), na.rm = TRUE),
      KWOKA = sum((c(top1, top2, top3) / total_top3)^2, na.rm = TRUE),
      .groups = "drop"
    )
    
    dat.k2$SUB <- "ALL"
    dat.k2$KWOKA[dat.k2$N < 3] <- 1
    
  dat.k3 <- 
    data.frame(SUB,YEAR,RES) %>%
    na.omit() %>%
    dplyr::filter(!is.na(RES)) %>%
    dplyr::group_by(SUB, YEAR) %>%
    dplyr::mutate(share = RES / sum(RES, na.rm = TRUE)) %>%  # normalize to shares
    dplyr::arrange(desc(share)) %>%
    dplyr::summarize(
      N = dplyr::n(),
      top1 = dplyr::nth(share, 1),
      top2 = dplyr::nth(share, 2),
      top3 = dplyr::nth(share, 3),
      total_top3 = sum(c(top1, top2, top3), na.rm = TRUE),
      KWOKA = sum((c(top1, top2, top3) / total_top3)^2, na.rm = TRUE),
      .groups = "drop"
    )
    
    dat.k3$GEO <- "ALL"
    dat.k3$KWOKA[dat.k3$N < 3] <- 1

    dat.k <- bind_rows( dat.k1, dat.k2, dat.k3 )
    dat.k <- arrange( dat.k, GEO, SUB, YEAR )
    
  return(dat.k)
}


# k <- get_kwoka_index(df)


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
#' get_kwoka_index_k(df, "RES", c("GEO", "SUB", "YEAR"), k = 3)
#'
#' @export
get_kwoka_k <- function(data, share_var, group_vars, k = 3) {

  share_sym <- rlang::sym(share_var)

  data %>%
    dplyr::filter(!is.na(!!share_sym)) %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
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

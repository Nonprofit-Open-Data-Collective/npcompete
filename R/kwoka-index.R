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

get_kwoka_index <- function(df, geo, subsector, resource){

  df <- df %>% rename(geo = {{geo}},
                      subsector = {{subsector}},
                      resource = {{resource}})


  if( any( df$resource < 0 ) ) warning("negative values replaced with zero.")
  dat.kwoka <-
    df %>%
    dplyr::mutate( geo=factor(geo),
                   subsector=factor(subsector),
                   resource = as.numeric(resource))%>%
    dplyr::group_by( geo, subsector ) %>%
    dplyr::mutate(rank = rank(desc(resource))) %>%
    dplyr::arrange(rank) %>%
    dplyr::summarize( n = dplyr::n(),
                      top1 = sum(nth(resource, 1)),
                      top2 = sum(nth(resource, 2)),
                      top3 = sum(nth(resource, 3)),
                      total = top1 + top2 + top3,
                      kindex = (((top1/total)^2 +(top2/total)^2+(top3/total)^2)))

  dat.kwoka$kindex[dat.kwoka$n < 3] <- 1
  
  return (dat.kwoka)
}

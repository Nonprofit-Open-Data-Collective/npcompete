###---------------------------------------------------
###   Herfindahl–Hirschman Index
###---------------------------------------------------

#' @title
#' Herfindahl–Hirschman Index
#'
#' @description
#' Calculate the Herfindahl–Hirschman Index (HHI) and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(hhi) grouped by geographical level and sub sector.
#' @param geo A character string indicating the column name for geographical area unique code.
#' @param subsector A character string indicating the column name for sub sectors.
#' @param resource A character string indicating the column name for resource based on which HHI is calculated. Example: Revenue, assets etc.
#' @param resource.name A character string indicating the column name for the new column generated with aggregated resource.
#'
#' @return A new dataframe with the Herfindahl–Hirschman Index (`hhi`),
#'  grouped by geographical level and sub sectors.
#'
#' @details The Herfindahl–Hirschman Index(HHI) is a measure of market concentration that is used to understand market competitiveness,
#'  competitive behavior and performance in markets. The index could be calculated as the sum of the squared market share of each firm
#'  competing in a market (Thornton & Belski, 2010).In the nonprofit sector, market share is determined by the ratio of a nonprofit's
#'  total revenue to the aggregate revenue for the organization’s market. The formula used here is \eqn{{\sigma \frac{resource}{Total resource}}^2}
#'  The calculated values range from 1/N to 1, where N is the number of organizations in a market. A market with an HHI of 1 is considered as a monopoly.
#'  In contrast, a market with an HHI close to 0 would be viewed as a nearly competition one.
#'
#' @examples
#' data(nonprofit_sample)
#  dat.hhi <-get_hhi( df=nonprofit_sample,'MSA_NECH','NTMAJ12','TOTREV', 'Revenue')
#' head( dat.hhi )
#'
#' @export
get_hhi <- function(df, geo, subsector, resource, resource.name='resource'){
  
  df <- df %>% rename(geo = {{geo}},
                      subsector = {{subsector}},
                      resource = {{resource}})
  
  if( any( df$resource < 0 ) ) warning("Negative values replaced with zero.")
  dat.hhi <-
    df %>%
    dplyr::mutate( geo=factor(geo),
                   subsector=factor(subsector),
                   resource = as.numeric(resource),
                   resource=bottomcode(resource) ) %>%
    dplyr::group_by( geo, subsector ) %>%
    dplyr::summarize( hhi= sum( ( resource / sum(resource))^2 ),
                      n=dplyr::n(),
                      {{resource.name}} := sum(resource) )
  
  dat.hhi$hhi[is.na(dat.hhi$hhi)] <- 0
  return (dat.hhi)
}
#' @title
#' Normalized Herfindahl–Hirschman Index
#'
#' @description
#' Calculate the normalized Herfindahl–Hirschman Index (HHI) and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(hhi) grouped by geographical level and sub sector.
#' @param geo A character string indicating the column name for geographical area unique code.
#' @param subsector A character string indicating the column name for sub sectors.
#' @param resource A character string indicating the column name for resource based on which HHI is calculated. Example: Revenue, assets etc.
#' @param normalizer A character string indicating the column name for the column used to normalize the HHI index.
#' @param resource.name A character string indicating the column name for the new column generated with aggregated resource.
#'
#' @return A new dataframe with the normalized Herfindahl–Hirschman Index (`hhi`),
#'  grouped by geographical level and sub sectors.
#'
#' @details The Herfindahl–Hirschman Index(HHI) is a measure of market concentration that is used to understand market competitiveness,
#'  competitive behavior and performance in markets. The index could be calculated as the sum of the squared market share of each firm
#'  competing in a market (Thornton & Belski, 2010).In the nonprofit sector, market share is determined by the ratio of a nonprofit's
#'  total revenue to the aggregate revenue for the organization’s market.The formula used here is \eqn{{\sigma \frac{resource}{Total resource}}^2 - \frac{\frac{1}{normalizer}}{1-\frac{1}{normalizer}}}
#'  The calculated values range from 1/N to 1, where N is the number of organizations in a market. A market with an HHI of 1 is considered as
#'  a monopoly. In contrast, a market with an HHI close to 0 would be viewed as a nearly competition one.
#'
#' @examples
#' data(nonprofit_sample)
#  dat.nhhi <-get_nhhi( df=nonprofit_sample,'MSA_NECH','NTMAJ12','EIN', TOTREV', 'Revenue')
#' head( dat.nhhi )
#'
#' @export
get_nhhi <- function(df, geo, subsector, resource, normalizer, resource.name = 'resource'){
  
  df <- df %>% rename(geo = {{geo}},
                      subsector = {{subsector}},
                      resource = {{resource}},
                      normalizer = {{normalizer}})
  
  
  if( any( df$resource < 0 ) ) warning("Negative values replaced with zero.")
  dat.nhhi <-
    df %>%
    dplyr::mutate( geo=factor(geo),
                   subsector=factor(subsector),
                   resource = as.numeric(resource),
                   resource=bottomcode(resource),
                   normalizer = as.numeric(normalizer)) %>%
    dplyr::group_by( geo, subsector ) %>%
    dplyr::summarize( nhhi= sum( ( resource / sum(resource) )^2 -
                                   ((1 / normalizer) / (1- ( 1 / normalizer)) )),
                      n=dplyr::n(),
                      {{resource.name}} := sum(resource) )
  
  dat.nhhi$nhhi[is.na(dat.nhhi$nhhi)] <- 0
  return (dat.nhhi)
}

#' @keywords internal
bottomcode <- function(x)
{
  x[ x < 0 ] <- 0
  return(x)
}

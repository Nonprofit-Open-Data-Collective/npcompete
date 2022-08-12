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
get_cr <- function(df, geo, subsector, resource, number){
  df <- df %>% rename(geo = {{geo}},
                      subsector = {{subsector}},
                      resource = {{resource}})

  if( any( df$resource < 0 ) ) warning("Negative values replaced with zero.")
  if(number > 8) warning("Number > 8!! Only top 8 values calculated. ")
  cratio.name <- paste0( 'CR', number )

  dat.cr <-
    df %>%
    dplyr::mutate( geo=factor(geo),
                   subsector=factor(subsector),
                   resource = as.numeric(resource),
                   resource=bottomcode(resource) )%>%
    dplyr::group_by( geo, subsector ) %>%
    dplyr::mutate(rank = rank(desc(resource))) %>%
    dplyr::arrange(rank) %>%
    dplyr::summarize(n = dplyr::n(),
                     top1 = sum(nth(resource, 1)),
                     top2 = sum(nth(resource, 2)),
                     top3 = sum(nth(resource, 3)),
                     top4 = sum(nth(resource, 4)),
                     top5 = sum(nth(resource, 5)),
                     top6 = sum(nth(resource, 6)),
                     top7 = sum(nth(resource, 7)),
                     top8 = sum(nth(resource, 8)),
                     total = sum(resource))
  
  dat.cr$cr <- rowSums(dat.cr[,4:(3+{{number}})]) / dat.cr$total
  dat.cr <- dat.cr %>%
    dplyr::mutate(cr = ifelse(n <= {{number}},100,cr)) %>%
    dplyr::rename(!!cratio.name := cr)
  
  return (dat.cr)
}


#' @keywords internal
bottomcode <- function(x)
{
  x[ x < 0 ] <- 0
  return(x)
}


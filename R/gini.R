###---------------------------------------------------
###   Gini Coefficient
###---------------------------------------------------

#' @title
#' Gini Coefficient
#'
#' @description
#' Calculate the Gini Coefficient and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(gini) grouped by geographical level and sub sector.
#' @param geo A character string indicating the column name for geographical area unique code.
#' @param subsector A character string indicating the column name for sub sectors.
#' @param resource A character string indicating the column name for resource based on which gini is calculated. Example: Revenue, assets etc.
#' @param resource.name A character string indicating the column name for the new column generated with aggregated resource.
#' @param fips A character string indicating the column name for FIPS code of the geographical area
#' @param year A numeric parameter indicating the year to be used for population estimate and metric evaluation for that year. Example: 1999
#' @param resource.smaller.than A numeric value indicating that it is the upper limit of the resource for density evaluation. All organizations having resource value less than that would be counted for evaluation.
#'
#' @return A new dataframe with the Gini Coefficient (`gini`),
#'  grouped by geographical level and sub sectors.
#'
#' @details The Gini coefficient has been widely used in economics as a measure of income inequality. 
#' Specifically, it measures income distribution among a population. In economics, there are multiple formulas 
#' used to calculate the coefficient (Dorfman, 1979). However, no matter which formula is selected, a low value 
#' of the coefficient indicates a high level of competition (less inequality); a high value represents a low 
#' level of competition (greater inequality). In nonprofit study, Seaman et al., (2014) supplemented HHI with 
#' the Gini coefficient to capture the concept of market competition and suggested that a market is considered 
#' as a reasonable level of equality if the coefficient is less than 0.5; a market is considered as a modest 
#' level of inequality if the coefficient is between 0.5 and 0.75; a market is considered as a high level of 
#' inequality if the coefficient is greater than .75.
#'
#' @examples
#' data(nonprofit_sample)
#  dat.gini <-get_gini( df=nonprofit_sample,'MSA_NECH','NTMAJ12','TOTREV')
#' head( dat.gini )
#'
#' @export
get_gini <- function(df, geo, subsector, resource){
  
  df <- df %>% rename(geo = {{geo}},
                      subsector = {{subsector}},
                      resource = {{resource}})
  
  if( any( df$resource < 0 ) ) warning("Negative values replaced with zero.")
  dat.gini <-
    df %>%
    dplyr::mutate( geo=factor(geo),
                   subsector=factor(subsector),
                   resource = as.numeric(resource),
                   resource=bottomcode(resource) ) %>%
    dplyr::arrange(resource) %>%
    dplyr::group_by( geo, subsector ) %>%
    dplyr::summarize( n=dplyr::n(),
                      gini = DescTools::Gini(resource))
  
  dat.gini$gini[dat.gini$n == 1] <- 1

  return (dat.gini)
}

#' @keywords internal
bottomcode <- function(x)
{
  x[ x < 0 ] <- 0
  return(x)
}
###---------------------------------------------------
###   Fundraising Efficiency
###---------------------------------------------------
#' @title
#' Fundraising efficiency
#'
#' @description
#' Calculate the fundraising efficiency of nonprofits in a geographical level  and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(fundraising efficiency) grouped by geographical level and sub sector.
#' @param geo A character string indicating the column name for geographical area unique code.
#' @param subsector A character string indicating the column name for sub sectors.
#' @param resource A character string indicating the column name for resource based on which fundraising efficiency is evaluated
#' @param list.of.cols A vector containing list of column names that have the fundraising fees for the nonprofits.
#'
#' @return A new dataframe with the fundraising efficiency of nonprofits, grouped by geographical level and sub sectors.
#'
#' @details Fundraising efficiency is the measure of the fundraising competition in a market. It represents total private donations raised relative 
#' to expenses on fundraising activities and indicates the amount of private donations raised for each dollar of fundraising expense incurred. 
#' Lower ratio indicates greater fundraising competition in a market.Fundraising efficiency be calculated as follows: Private donations divided by fundraising expenses  
#'
#' @examples
#' data(nonprofit_sample)
#' dat.fe <- get_density_commercial( df=nonprofit_sample,'MSA_NECH','NTMAJ12', 'CONT', c('FUNDFEES', 'SOLICIT'))
#' head( dat.fe)
#'
#' @export
get_fundraising_efficiency <- function(df, geo, subsector, resource, list.of.cols){
  
  df <- df %>%
    rename(geo = {{geo}},
           subsector = {{subsector}},
           resource = {{resource}})
  
  df$fundraisingexp <- 0
  df$resource <- as.numeric(df$resource)
  
  for (colname in list.of.cols){
    df[[colname]] <- as.numeric(df[[colname]])
  }
  
  
  for (colname in list.of.cols){
    df$fundraisingexp <- rowSums(df[,c('fundraisingexp', colname)], na.rm = TRUE)
  }
  
  
  dat.fe <-
    df %>%
    dplyr::mutate( geo=factor(geo),
                   subsector=factor(subsector)) %>%
    dplyr::group_by( geo, subsector ) %>%
    dplyr::summarize(n = dplyr::n(),
                     resource_total = sum(resource),
                     fe <- (resource_total) / fundraisingexp )
  
  return (dat.fe)
}
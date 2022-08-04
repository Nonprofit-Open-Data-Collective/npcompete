###---------------------------------------------------
###   Get all metrics
###---------------------------------------------------

#' @title
#' Get all metrics
#'
#' @description
#' Calculate all competition metrics and return in a single dataframe and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric grouped by geographical level and sub sector.
#' @param geo A character string indicating the column name for geographical area unique code.
#' @param subsector A character string indicating the column name for sub sectors.
#' @param resource A character string indicating the column name for resource based on which HHI is calculated. Example: Revenue, assets etc.
#' @param resource.name A character string indicating the column name for the new column generated with aggregated resource.
#' @param normalizer A character string indicating the column name for the column used to normalize the HHI index.
#' @param number A numeric value that is used to calculate concentration ratio for top that number of firms. Value ranges from 1-8. Example, 4 if concentration ratio for top 4 firms.
#' @param smaller.than A numeric value indicating that it is the upper limit of the resource for density evaluation. All organizations having resource value less than that would be counted for evaluation.
#' @param resource.greater.than A numeric value indicating that it is the lower limit of the resource for density evaluation. All organizations having resource value greater than that would be counted for evaluation.
#' @param fundraising.cols A vector containing list of column names that have the fundraising fees for the nonprofits.#' 
#' 
#'
#' @return A new dataframe with all metrics evaluated for each year.
#'
#' @examples
#  dat.all <- get_all_metrics(nonprofit_sample,'MSA_NECH','NTMAJ12','TOTREV', 'Revenue', 'EIN',4, 'FIPS', 2000, 100000, 100000, col.names)
#' head( dat.all )
#'
#' @export
get_all_metrics <- function(df, geo, subsector, resource, resource.name, normalizer, fips, dat.year, resource.smaller.than, resource.greater.than, fundraising.cols ){
  
  
  dat.hhi <- get_hhi(df, {{geo}}, subsector, resource, resource.name)
  dat.hhi <- select(dat.hhi, geo, subsector, hhi,n)
   
  dat.nhhi <- get_nhhi(df, geo, subsector, resource, normalizer)
  dat.nhhi <- select(dat.nhhi, geo, subsector, nhhi)

  
  crlist = list()
  for (num in 1:8){
    dat.cr <- get_cr(df, geo, subsector, resource, num)
    crlist[[num]] <- dat.cr 
  } 
  dat.cr <- Reduce(merge, crlist)
  drop.cols <- c("top1","top2","top3","top4","top5","top6","top7","top8","total")
  dat.cr <- dat.cr[ , !(names(dat.cr) %in% drop.cols)]

  dat.kindex <- get_kwoka_index(df, geo, subsector, resource)
  dat.kindex <- select(dat.kindex, geo, subsector, kindex)
  
  
  dat.density <- get_density(df, geo, subsector, all_of(fips) , dat.year)
  dat.density <- select(dat.density, geo, subsector, densityper100000)
  
  
  dat.density.smallerthan <- get_density_small(df, geo, subsector, all_of(fips) , dat.year, resource, resource.smaller.than )
  dat.density.smallerthan <- select(dat.density.smallerthan, geo, subsector, densitysmallper100000 )
  
  dat.density.greaterthan <- get_density_big(df, geo, subsector, all_of(fips) , dat.year, resource, resource.greater.than )
  dat.density.greaterthan <- select(dat.density.greaterthan, geo, subsector, densitybigper100000 )
  
  
  dat.gini <- get_gini(df,geo, subsector, resource)
  dat.gini <- select(dat.gini, geo, subsector, gini)
  
  
  dat.density.comm <- get_density_commercial(df, geo, subsector, resource, fundraising.cols)
  dat.density.comm <- select(dat.density.comm, geo, subsector, density_commercial)
  
  dat.all <- Reduce(merge, list(dat.hhi, dat.nhhi, dat.cr, dat.kindex, dat.gini, dat.density, dat.density.smallerthan, dat.density.greaterthan, dat.density.comm))
  dat.all$year <- dat.year

  return (dat.all)
} 

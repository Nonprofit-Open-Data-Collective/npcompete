###---------------------------------------------------
###   Density
###---------------------------------------------------

#' @title
#' Density metric
#'
#' @description
#' Calculate the nonprofit density metric and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(density) grouped by gepgraphical level and sub sector.
#' @param geo A character string indicating the column name for geographical area unique code.
#' @param subsector A character string indicating the column name for sub sectors.
#' @param fips A character string indicating the column name for FIPS code of the geographical area
#' @param year A numeric parameter indicating the year to be used for population estimate. Example: 1999
#'
#' @return A new dataframe with the density of nonprofits per 100000 population,
#'  grouped by geographical level and sub sectors.
#'
#' @details The density metric is the number of nonprofits per-capita in a market. In this function, it is measured
#' as the number of organizations per 100,000 population.
#'
#' @examples
#' data(county_population)
#' data(nonprofit_sample)
#  dat.den.2009 <- get_density( df=nonprofit_sample,'MSA_NECH','NTMAJ12','FIPS', 2009)
#' head( dat.den.2009 )
#'
#' @export
get_density <- function(df, geo, subsector, fips, year){

  core <- select( df, {{geo}}, {{subsector}}, {{fips}})
  core <- core %>% rename(geo = {{geo}},
                          subsector = {{subsector}},
                          FIPS = {{fips}})


  core$FIPS <- as.numeric( core$FIPS )
  core <- core %>% mutate(FIPS = sprintf("%05d", FIPS))

  col.name <- paste0("POPESTIMATE", {{year}})

  pop <- select(county_population, {{fips}}, {{col.name}})
  pop <- pop %>% rename(FIPS = {{fips}},
                        countypop = {{col.name}})
  pop <- pop %>% mutate(FIPS = sprintf("%05d", FIPS))

  den.data <- merge( core, pop, by.x="FIPS",by.y = "FIPS" , all.x=T )

  dat.density <-
    den.data %>%
    select(geo, subsector, countypop) %>%
    dplyr::mutate( geo=factor(geo),
                   subsector=factor(subsector)) %>%
    dplyr::group_by( geo, subsector ) %>%
    dplyr::summarize( n=dplyr::n(),
                      geopop = sum(countypop),
                      densityper100000 = n / (geopop/100000))


  return (dat.density)
}


#' @title
#' Density small
#'
#' @description
#' Calculate the nonprofit density metric having resource smaller than a numeric value and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(density) grouped by gepgraphical level and sub sector.
#' @param geo A character string indicating the column name for geographical area unique code.
#' @param subsector A character string indicating the column name for sub sectors.
#' @param fips A character string indicating the column name for FIPS code of the geographical area
#' @param year A numeric parameter indicating the year to be used for population estimate. Example: 1999
#' @param resource A character string indicating the column name for resource based on which density is evaluated
#' @param smaller.than A numeric value indicating that it is the upper limit of the resource for density evaluation. All organizations having resource value less than that would be counted for evaluation.
#'
#' @return A new dataframe with the density of nonprofits per 100000 population, having resource smaller than the given number
#'  grouped by geographical level and sub sectors.
#'
#' @details The density metric is the number of nonprofits per-capita in a market. In this function, it is measured
#' as the number of organizations per 100,000 population.
#'
#' @examples
#' data(county_population)
#' data(nonprofit_sample)
#' dat.den.2009 <- get_density_small( df=nonprofit_sample,'MSA_NECH','NTMAJ12','FIPS', 2009, 'TOTREV', 100000)
#' head( dat.den.2009 )
#'
#' @export
get_density_small <- function(df, geo, subsector, fips, year, resource, smaller.than){

  core <- select( df, {{geo}}, {{subsector}}, {{fips}}, {{resource}})
  core <- core %>% rename(geo = {{geo}},
                          subsector = {{subsector}},
                          FIPS = {{fips}},
                          resource = {{resource}})


  core$FIPS <- as.numeric( core$FIPS )
  core <- core %>% mutate(FIPS = sprintf("%05d", FIPS))

  col.name <- paste0("POPESTIMATE", {{year}})

  pop <- select(county_population, {{fips}}, {{col.name}})
  pop <- pop %>% rename(FIPS = {{fips}},
                        countypop = {{col.name}})
  pop <- pop %>% mutate(FIPS = sprintf("%05d", FIPS))

  den.data <- merge( core, pop, by.x="FIPS",by.y = "FIPS" , all.x=T )

  dat.density <-
    den.data %>%
    select(geo, subsector, resource, countypop) %>%
    dplyr::mutate( geo=factor(geo),
                   subsector=factor(subsector),
                   resource = as.numeric(resource)) %>%
    dplyr::group_by( geo, subsector ) %>%
    dplyr::summarize( n=sum(resource < {{smaller.than}}),
                      geopop = sum(countypop),
                      densitysmallper100000 = n / (geopop/100000))


  return (dat.density)
}



#' @title
#' Density big
#'
#' @description
#' Calculate the nonprofit density metric having resource larger than a numeric value and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(density) grouped by geographical level and sub sector.
#' @param geo A character string indicating the column name for geographical area unique code.
#' @param subsector A character string indicating the column name for sub sectors.
#' @param fips A character string indicating the column name for FIPS code of the geographical area
#' @param year A numeric parameter indicating the year to be used for population estimate. Example: 1999
#' @param resource A character string indicating the column name for resource based on which density is evaluated
#' @param greater.than A numeric value indicating that it is the lower limit of the resource for density evaluation. All organizations having resource value greater than that would be counted for evaluation.
#'
#' @return A new dataframe with the density of nonprofits per 100000 population, having resource greater than the given number
#'  grouped by geographical level and sub sectors.
#'
#' @details The density metric is the number of nonprofits per-capita in a market. In this function, it is measured
#' as the number of organizations per 100,000 population.
#'
#' @examples
#' data(county_population)
#' data(nonprofit_sample)
#' dat.den.2009 <- get_density_big( df=nonprofit_sample,'MSA_NECH','NTMAJ12','FIPS', 2009, 'TOTREV', 100000)
#' head( dat.den.2009 )
#'
#' @export
get_density_big <- function(df, geo, subsector, fips, year, resource, greater.than){

  core <- select( df, {{geo}}, {{subsector}}, {{fips}}, {{resource}})
  core <- core %>% rename(geo = {{geo}},
                          subsector = {{subsector}},
                          FIPS = {{fips}},
                          resource = {{resource}})


  core$FIPS <- as.numeric( core$FIPS )
  core <- core %>% mutate(FIPS = sprintf("%05d", FIPS))

  col.name <- paste0("POPESTIMATE", {{year}})


  pop <- select(county_population, {{fips}}, {{col.name}})
  pop <- pop %>% rename(FIPS = {{fips}},
                        countypop = {{col.name}})
  pop <- pop %>% mutate(FIPS = sprintf("%05d", FIPS))

  den.data <- merge( core, pop, by.x="FIPS",by.y = "FIPS" , all.x=T )

  dat.density <-
    den.data %>%
    select(geo, subsector, resource, countypop) %>%
    dplyr::mutate( geo=factor(geo),
                   subsector=factor(subsector),
                   resource = as.numeric(resource)) %>%
    dplyr::group_by( geo, subsector ) %>%
    dplyr::summarize( n=sum(resource > {{greater.than}}),
                      geopop = sum(countypop),
                      densitybigper100000 = n / (geopop/100000))


  return (dat.density)
}


#' @title
#' Density commercial
#'
#' @description
#' Calculate the commercial nonprofit density  a resource  and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(density commercial) grouped by geographical level and sub sector.
#' @param geo A character string indicating the column name for geographical area unique code.
#' @param subsector A character string indicating the column name for sub sectors.
#' @param resource A character string indicating the column name for resource based on which density is evaluated
#' @param list.of.cols A vector containing list of column names that have the commercial metrics for the resource
#'
#' @return A new dataframe with the commercial density of nonprofits summarized in the subsector and geo level.
#'
#' @details The density metric is the number of nonprofits per-capita in a market. In this function, it is measured
#' as the number of organizations per 1000 population and per 10,000 population.
#'
#' @examples
#' data(county_population)
#' data(nonprofit_sample)
#' dat.den.comm <- get_density_commercial( df=nonprofit_sample,'MSA_NECH','NTMAJ12', 'TOTREV', c('PROGREV', 'INVINC'))
#' head( dat.den.comm )
#'
#' @export
get_density_commercial <- function(df, geo, subsector, resource, list.of.cols){

  df <- df %>%
        rename(geo = {{geo}},
                      subsector = {{subsector}},
                      resource = {{resource}})

  df$commercialres <- 0
  df$resource <- as.numeric(df$resource)

  for (colname in list.of.cols){
    df[[colname]] <- as.numeric(df[[colname]])

  }


  for (colname in list.of.cols){
    df$commercialres <- rowSums(df[,c('commercialres', colname)], na.rm = TRUE)
  }

  df$commercialperc <- (df$commercialres / df$resource) *100
  df$commercialperc[df$resource == 0] <- 0
  

  dat.comm <-
    df %>%
    dplyr::mutate( geo=factor(geo),
                   subsector=factor(subsector)) %>%
    dplyr::group_by( geo, subsector ) %>%
    dplyr::summarize(n = dplyr::n(),
                     resource_total = sum(resource),
                     n_commercial = sum(commercialperc > 50),
                     density_commercial = (n_commercial / n)*100 )

  return (dat.comm)

}






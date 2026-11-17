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
get_gini <- function(df, geography="CENSUS_CBSA_FIPS", subsector="NTEEV2", resource="TOTREV", year="TAXYEAR"){
  
  GEO <- df[[geography]] |> factor( exclude = NULL )
  RES <- df[[resource]]  |> as.numeric() |> suppressWarnings()
  if( subsector == "NTEEV2" )
  { SUB <- df[[subsector]] |> substr(1,3) |> factor( exclude = NULL ) } else
  { SUB <- df[[subsector]] |> factor( exclude = NULL ) }

  RES <- bottomcode(RES)

  if( ! is.null(year) ){ 

    YEAR <- df[[year]] |> factor() 
  
    dat.gini1 <-
      data.frame(GEO,SUB,YEAR,RES) %>%
      na.omit() %>%
      dplyr::arrange(RES) %>%
      dplyr::group_by( GEO, SUB, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        {{resource}} := sum(RES),
                        GINI = DescTools::Gini(RES),
                        .groups = "drop" ) 

    dat.gini2 <-
      data.frame(GEO,YEAR,RES) %>%
      na.omit() %>%
      dplyr::arrange(RES) %>%
      dplyr::group_by( GEO, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        {{resource}} := sum(RES),
                        GINI = DescTools::Gini(RES),
                        .groups = "drop" ) 
    dat.gini2$SUB <- "ALL"

    dat.gini3 <-
      data.frame(SUB,YEAR,RES) %>%
      na.omit() %>%
      dplyr::arrange(RES) %>%
      dplyr::group_by( SUB, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        {{resource}} := sum(RES),
                        GINI = DescTools::Gini(RES),
                        .groups = "drop" )

    dat.gini3$GEO <- "ALL"

    dat.gini <- bind_rows( dat.gini1, dat.gini2, dat.gini3 )
    dat.gini <- arrange( dat.gini, GEO, SUB, YEAR )
    
  } # end of if

  if( is.null(year) ){ 

    dat.gini <-
      data.frame(GEO,SUB,RES) %>%
      na.omit() %>%
      dplyr::arrange(RES) %>%
      dplyr::group_by( GEO, SUB ) %>%
      dplyr::summarize( N=dplyr::n(),
                        {{resource}} := sum(RES),
                        GINI = DescTools::Gini(RES),
                        .groups = "drop" )
  } # end of if

  dat.gini$GINI[ dat.gini$N == 1] <- 1
  dat.gini$GINI[ is.nan(dat.gini$GINI) ] <- NA

  return (dat.gini)
}

#' @keywords internal
bottomcode <- function(x)
{
  x[ x < 0 ] <- 0
  return(x)
}


# gini <- get_gini( df3 )
# gini2 <- filter( gini, N > 1 & YEAR %in% 2011:2022 )
#
# t <- tapply( gini2$GINI, gini2$N, median )
# plot( log10(as.numeric(names(t))), t, pch=19, col=gray(0.5,0.5),
#       xlab="Number of Nonprofits (log10)", ylab="Gini Coefficient",
#       bty="n", xlim=c(0.5,4) )
#
# t <- tapply( gini2$GINI, gini2$YEAR, median, na.rm=T ) 
# plot( as.numeric(names(t)), t, pch=19, type="b",
#       xlab="Year", ylab="Gini Coefficient",
#       bty="n", cex=2 )
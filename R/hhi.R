###---------------------------------------------------
###   Herfindahl–Hirschman Index
###---------------------------------------------------

#' Herfindahl–Hirschman Index
#'
#' Market concentration in each geography, subsector and year.
#'
#' @inheritParams npcompete-params
#' @param year Name of the year column, or `NULL` to pool all years.
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR` (unless `year = NULL`), `N`, the
#'   summed resource and `HHI`.
#'
#' @details The Herfindahl–Hirschman Index(HHI) is a measure of market concentration that is used to understand market competitiveness,
#'  competitive behavior and performance in markets. The index could be calculated as the sum of the squared market share of each firm
#'  competing in a market (Thornton & Belski, 2010).In the nonprofit sector, market share is determined by the ratio of a nonprofit's
#'  total revenue to the aggregate revenue for the organization’s market. The formula used here is \eqn{{\sigma \frac{resource}{Total resource}}^2}
#'  The calculated values range from 1/N to 1, where N is the number of organizations in a market. A market with an HHI of 1 is considered as a monopoly.
#'  In contrast, a market with an HHI close to 0 would be viewed as a nearly competition one.
#'
#' @examples
#' \dontrun{
#' get_hhi( df, resource = "TOTREV" )
#' }
#'
#' @export
get_hhi <- function(df, geography="CENSUS_CBSA_FIPS", subsector="NTEEV2", resource="TOTREV", year="TAXYEAR"){

  GEO <- df[[geography]] |> factor( exclude = NULL )
  RES <- df[[resource]]  |> as.numeric() |> suppressWarnings()
  if( subsector == "NTEEV2" )
  { SUB <- df[[subsector]] |> substr(1,3) |> factor( exclude = NULL ) } else
  { SUB <- df[[subsector]] |> factor( exclude = NULL ) }

  if( any( RES < 0 ) ) warning("Negative resource values replaced with zero.")
  RES <- bottomcode(RES)

  if( ! is.null(year) ){ 

    YEAR <- df[[year]] |> factor() 
  
    dat.hhi <-
      data.frame(GEO,SUB,YEAR,RES) %>%
      stats::na.omit() %>%
      dplyr::arrange(RES) %>%
      dplyr::group_by( GEO, SUB, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        {{resource}} := sum(RES),
                        HHI= sum( ( RES / sum(RES))^2 ),
                        .groups = "drop" )
  }
  if( is.null(year) ){ 
  
    dat.hhi <-
      data.frame(GEO,SUB,RES) %>%
      stats::na.omit() %>%
      dplyr::arrange(RES) %>%
      dplyr::group_by( GEO, SUB ) %>%
      dplyr::summarize( N=dplyr::n(),
                        {{resource}} := sum(RES),
                        HHI= sum( ( RES / sum(RES))^2 ),
                        .groups = "drop" )
  }

  dat.hhi$HHI[is.na(dat.hhi$HHI)] <- 0
  dat.hhi$HHI[is.nan(dat.hhi$HHI)] <- NA
  return (dat.hhi)
}



#' Normalized Herfindahl–Hirschman Index
#'
#' HHI rescaled so that it runs from 0 to 1 whatever the number of
#' organizations, computed for each geography, subsector and year, with
#' rollups for all subsectors (`SUB = "ALL"`) and all geographies
#' (`GEO = "ALL"`).
#'
#' @inheritParams npcompete-params
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `N`, the summed resource, `HHI`
#'   and `NHHI`.
#'
#' @details `NHHI = (HHI - 1/N) / (1 - 1/N)`, where `N` is the number of
#'   organizations in the market. Markets with one organization get
#'   `NHHI = 0`.
#'
#' @examples
#' \dontrun{
#' get_nhhi( df, resource = "TOTREV" )
#' }
#'
#' @export
get_nhhi <- function(df, geography="CENSUS_CBSA_FIPS", subsector="NTEEV2", resource="TOTREV", year="TAXYEAR"){
  
  GEO <- df[[geography]] |> factor( exclude = NULL )
  RES <- df[[resource]]  |> as.numeric() |> suppressWarnings()
  if( subsector == "NTEEV2" )
  { SUB <- df[[subsector]] |> substr(1,3) |> factor( exclude = NULL ) } else
  { SUB <- df[[subsector]] |> factor( exclude = NULL ) }

  RES <- bottomcode(RES)
  
  YEAR <- df[[year]] |> factor() 
  
    nhhi1 <-
      data.frame(GEO,SUB,YEAR,RES) %>%
      stats::na.omit() %>%
      dplyr::arrange(RES) %>%
      dplyr::group_by( GEO, SUB, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        {{resource}} := sum(RES),
                        HHI= sum( ( RES / sum(RES))^2 ),
                        NHHI= calc_nhhi(RES),
                        .groups = "drop" )

    nhhi2 <-
      data.frame(GEO,YEAR,RES) %>%
      stats::na.omit() %>%
      dplyr::arrange(RES) %>%
      dplyr::group_by( GEO, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        {{resource}} := sum(RES),
                        HHI= sum( ( RES / sum(RES))^2 ),
                        NHHI= calc_nhhi(RES),
                        .groups = "drop" )
    nhhi2$SUB <- "ALL" 
    
    nhhi3 <-
      data.frame(SUB,YEAR,RES) %>%
      stats::na.omit() %>%
      dplyr::arrange(RES) %>%
      dplyr::group_by( SUB, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        {{resource}} := sum(RES),
                        HHI= sum( ( RES / sum(RES))^2 ),
                        NHHI= calc_nhhi(RES),
                        .groups = "drop" )
    nhhi3$GEO <- "ALL" 

    nhhi <- bind_rows( nhhi1, nhhi2, nhhi3 )
    nhhi <- arrange( nhhi, GEO, SUB, YEAR )
    
  nhhi$NHHI[is.na(nhhi$NHHI)] <- 0
  return (nhhi)
}


# Normalized HHI for a vector of resource values (internal).
calc_nhhi <- function(x) {
  n <- length(x)
  s <- x / sum(x)
  hhi <- sum(s^2)
  nhhi <- (hhi - 1/n) / (1 - 1/n)
  return(nhhi)
}


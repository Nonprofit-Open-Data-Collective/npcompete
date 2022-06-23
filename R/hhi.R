###---------------------------------------------------
###   Herfindahl–Hirschman Index
###---------------------------------------------------

#' @title
#' Herfindahl–Hirschman Index
#'
#' @description
#' Calculate the Herfindahl–Hirschman Index (HHI) and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(hhi) grouped by MSA_NECH and sub sector.
#' @param MSA_NECH A character string indicating the column name for Metropolitan Statistical Area unique code.
#' @param NTMAJ12 A character string indicating the column name for sub sectors.
#' @param TOTREV A character string indicating the column name for total revenue.
#' @param CONT A character string indicating the column name for the contribution.
#'
#' @return A new dataframe with the Herfindahl–Hirschman Index (`hhi`),
#'  grouped by MSA_NECH and NTMAJ12(12 sub sectors).
#'
#' @details The Herfindahl–Hirschman Index(HHI) is a measure of market concentration that is used to understand market competitiveness,
#'  competitive behavior and performance in markets. The index could be calculated as the sum of the squared market share of each firm
#'  competing in a market (Thornton & Belski, 2010).In the nonprofit sector, market share is determined by the ratio of a nonprofit's
#'  total revenue to the aggregate revenue for the organization’s market. The calculated values range from 1/N to 1, where N is the number
#'  of organizations in a market. A market with an HHI of 1 is considered as a monopoly. In contrast, a market with an HHI close to 0 would
#'  be viewed as a nearly competition one.
#'
#' @examples
#' x1 <- floor(runif(5, min=10000, max=99999))
#  x1 <- append(x1,x1)
#  x2 <- c("Arts", "Health","Education","Environment","Arts", "Health","Education","Environment","Arts", "Health")
#  x3<-rnorm( 10,200,30 )
#  x4 <- rnorm(10,500,50)
#
#  dat<-data.frame( x1, x2, x3 ,x4)
#  colnames(dat) <- c('MSA_NECH','NTMAJ12','TOTREV', 'CONT')
#
#  a<-get_hhi( df=dat,'MSA_NECH','NTMAJ12','TOTREV', 'CONT')
#' head( a )
#'
#' @export
get_hhi <- function(df, MSA_NECH, NTMAJ12, TOTREV, CONT){

  dat.hhi <-
    df %>%
    group_by( MSA_NECH, NTMAJ12 ) %>%
    summarize( hhi= sum( (TOTREV / sum(TOTREV))^2 ),
               n=n(),
               revenue= sum(TOTREV),
               contribution=sum(CONT) )


  dat.hhi$hhi[ dat.hhi$hhi > 1 ] <- 1
  dat.hhi$revenue[ dat.hhi$revenue < 0 ] <- 0

  return (dat.hhi)
}

#' @title
#' Normalized Herfindahl–Hirschman Index
#'
#' @description
#' Calculate the normalized Herfindahl–Hirschman Index (HHI) and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(Total revenue) grouped by MSA_NECH and sub sector.,
#' @param MSA_NECH A character string indicating the column name for Metropolitan Statistical Area unique code.
#' @param NTMAJ12 A character string indicating the column name for sub sectors.
#' @param TOTREV A character string indicating the column name for total revenue.
#' @param CONT A character string indicating the column name for the contribution.
#'
#' @return A new dataframe with the normalized Herfindahl–Hirschman Index (`hhi`),
#'  grouped by MSA_NECH and NTMAJ12(12 sub sectors).
#'
#' @details The Herfindahl–Hirschman Index(HHI) is a measure of market concentration that is used to understand market competitiveness,
#'  competitive behavior and performance in markets. The index could be calculated as the sum of the squared market share of each firm
#'  competing in a market (Thornton & Belski, 2010).In the nonprofit sector, market share is determined by the ratio of a nonprofit's
#'  total revenue to the aggregate revenue for the organization’s market. The calculated values range from 1/N to 1, where N is the number
#'  of organizations in a market. A market with an HHI of 1 is considered as a monopoly. In contrast, a market with an HHI close to 0 would
#'  be viewed as a nearly competition one.
#'
#' @examples
#' x1 <- floor(runif(5, min=10000, max=99999))
#  x1 <- append(x1,x1)
#  x2 <- c("Arts", "Health","Education","Environment","Arts", "Health","Education","Environment","Arts", "Health")
#  x3<-rnorm( 10,200,30 )
#  x4 <- rnorm(10,500,50)
#
#  dat<-data.frame( x1, x2, x3 ,x4)
#  colnames(dat) <- c('MSA_NECH','NTMAJ12','TOTREV', 'CONT')
#
#  a<-get_nhhi( df=dat,'MSA_NECH','NTMAJ12','TOTREV', 'CONT')
#' head( a )
#'
#' @export
get_nhhi <- function(df, MSA_NECH, NTMAJ12, TOTREV, CONT){
  dat.nhhi <-
    df %>%
    group_by( MSA_NECH, NTMAJ12 ) %>%
    summarize(
      nhhi= sum((((TOTREV / sum(TOTREV))^2) -
                   (1 / EIN)) / (1 -(1 / EIN))),
      n=n(), revenue= sum(TOTREV))


  dat.nhhi$nhhi[ dat.nhhi$nhhi > 1 ] <- 1

  dat.nhhi$revenue[ dat.nhhi$revenue < 0 ] <- 0

  return (dat.nhhi)
}


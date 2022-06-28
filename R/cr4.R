###---------------------------------------------------
###   Concentration Ratio
###---------------------------------------------------

#' @title
#' Concentration Ratio -4
#'
#' @description
#' Calculate the Concentration Ratio (CR) of top 4 largest organizations and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(cr4) grouped by MSA_NECH and sub sector.
#' @param MSA_NECH A character string indicating the column name for Metropolitan Statistical Area unique code.
#' @param TOTREV A character string indicating the column name for total revenue.
#' @param NTMAJ12 A character string indicating the column name for sub sectors.
#'
#' @return A new dataframe with the Concentration Ratio of 4 largest organizations ('cr4'),
#'  grouped by MSA_NECH. *Note: For the markets where total number of firms is less than 4,
#'  the concentration ratio is assigned 100%.
#'
#' @details CR4 is defined as the summation of the market shares of the largest four firms.
#'  It is calculated using the summation of revenue of the largest four nonprofits divided by the summation of
#'  revenue of all nonprofits in a market.
#'
#' @examples
#' core.data <- read.csv( "data/sample.csv" )
#' core.data$TOTREV[ core.data$TOTREV < 0 ] <- 0
#
#  a<-get_cr4( df=dat,'MSA_NECH','NTMAJ12','TOTREV')
#' head( a )
#'
#' @export
get_cr4 <- function(df, MSA_NECH, NTMAJ12,TOTREV){
  dat.CR4 <-
    df %>%
    group_by( MSA_NECH) %>%
    mutate(rank = rank(desc(TOTREV))) %>%
    arrange(rank) %>%
    summarize( n=n(),NTMAJ12 = NTMAJ12, revenue= sum(TOTREV), top1 = sum(nth(TOTREV, 1)), top2 = sum(nth(TOTREV, 2)),
               top3 = sum(nth(TOTREV, 3)), top4 = sum(nth(TOTREV, 4)),
               percent=((top1+top2+top3+top4)/revenue) * 100) %>%
    mutate(percent = ifelse(n < 4,100,percent))

  return (dat.CR4)
}
###---------------------------------------------------
###   Concentration Ratio
###---------------------------------------------------

#' @title
#' Concentration Ratio -2
#'
#' @description
#' Calculate the Concentration Ratio (CR) of top 2 largest organizations and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(cr2) grouped by MSA_NECH and sub sector.,
#' @param MSA_NECH A character string indicating the column name for Metropolitan Statistical Area unique code.
#' @param TOTREV A character string indicating the column name for total revenue.
#' @param NTMAJ12 A character string indicating the column name for sub sectors.
#'
#' @return A new dataframe with the Concentration Ratio of 4 largest organizations ('cr4'),
#'  grouped by MSA_NECH.
#'
#' @details CR2 is defined as the summation of the market shares of the largest two firms.
#'  It is calculated using the summation of revenue of the largest two nonprofits divided by the summation of
#'  revenue of all nonprofits in a market.
#'
#' @examples
#' core.data <- read.csv( "data/sample.csv" )
#' core.data$TOTREV[ core.data$TOTREV < 0 ] <- 0
#
#  a<-get_cr4( df=dat,'MSA_NECH','NTMAJ12','TOTREV')
#' head( a )
#'
#' @export
get_cr2 <- function(df, MSA_NECH, NTMAJ12,TOTREV){
  dat.CR2 <-
    df %>%
    group_by( MSA_NECH) %>%
    mutate(rank = rank(desc(TOTREV))) %>%
    arrange(rank) %>%
    summarize( n=n(),NTMAJ12 = NTMAJ12, revenue= sum(TOTREV), top1 = sum(nth(TOTREV, 1)), top2 = sum(nth(TOTREV, 2)),
               percent=((top1+top2)/revenue)*100)%>%
    mutate(percent = ifelse(n < 2,100,percent))

  return (dat.CR2)
}

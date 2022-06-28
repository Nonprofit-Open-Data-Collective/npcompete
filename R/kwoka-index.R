###---------------------------------------------------
###   Kwoka Index
###---------------------------------------------------

#' @title
#' Kwoka Index
#'
#' @description
#' Calculate the Kwoka Index (kindex) and return a dataframe.
#'
#' @param df A dataframe containing the required fields for computing the metric(kwoka index) grouped by MSA_NECH and sub sector.
#' @param MSA_NECH A character string indicating the column name for Metropolitan Statistical Area unique code.
#' @param NTMAJ12 A character string indicating the column name for sub sectors.
#' @param TOTREV A character string indicating the column name for total revenue.
#'
#' @return A new dataframe with the Kwoka Index (`kindex`),
#'  grouped by MSA_NECH and NTMAJ12(12 sub sectors).
#'
#' @details The Kwoka Index(kindex) is a measure of market concentration that is used to understand market competitiveness,
#'  competitive behavior and performance in markets.
#'
#' @examples
#' core.data <- read.csv( "data/sample.csv" )
#' core.data$TOTREV[ core.data$TOTREV < 0 ] <- 0
#
#  a<-get_kwoka_index( df=dat,'MSA_NECH','NTMAJ12','TOTREV')
#' head( a )
#'
#' @export

get_kwoka_index <- function(df, MSA_NECH, NTMAJ12,TOTREV){
  dat.kwoka <- df %>%
    group_by( MSA_NECH) %>%
    mutate(rank = rank(desc(TOTREV))) %>%
    arrange(rank) %>%
    summarize( n=n(),NTMAJ12 = NTMAJ12, top1 = sum(nth(TOTREV, 1)), top2 = sum(nth(TOTREV, 2)),
               top3 = sum(nth(TOTREV, 3)),total = top1 + top2 + top3, Kindex = (((top1/total)^2 +
                                                                                    (top2/total)^2+(top3/total)^2)))

  return (dat.kwoka)
}

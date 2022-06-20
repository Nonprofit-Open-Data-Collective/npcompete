
hhi <- function(core.data){
  dat.hhi <-
    core.data %>%
    group_by( MSA_NECH, NTMAJ12 ) %>%
    summarize( hhi= sum( (TOTREV / sum(TOTREV))^2 ),
               n=n(),
               revenue= sum(TOTREV),
               contribution=sum(CONT) )


  dat.hhi$hhi[ dat.hhi$hhi > 1 ] <- 1
  dat.hhi$revenue[ dat.hhi$revenue < 0 ] <- 0

  return (dat.hhi)
}

nhhi <- function(core.data){
  dat.nhhi <-
    core.data %>%
    group_by( MSA_NECH, NTMAJ12 ) %>%
    summarize(
      nhhi= sum((((TOTREV / sum(TOTREV))^2) -
                   (1 / EIN)) / (1 -(1 / EIN))),
      n=n(), revenue= sum(TOTREV))


  dat.nhhi$nhhi[ dat.nhhi$nhhi > 1 ] <- 1

  dat.nhhi$revenue[ dat.nhhi$revenue < 0 ] <- 0

  return (dat.nhhi)
}


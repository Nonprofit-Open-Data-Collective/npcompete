
library(MASS)
library(dplyr)
library(tidyr)
library(purrr)


df <- dplyr::filter( df3, TAXYEAR > 2010 & TAXYEAR < 2023 )
df <- dplyr::filter( df, V501C == 3 )



get_scale_free_density <- function(df, geography="CENSUS_CBSA_FIPS", population="POP_METRO", subsector="NTEEV2", year="TAXYEAR", resource="TOTREV"){
  
  GEO <- df[[geography]] |> factor( exclude = NULL )
  if( subsector == "NTEEV2" )
  { SUB <- df[[subsector]] |> substr(1,3) |> factor( exclude = NULL ) } else
  { SUB <- df[[subsector]] |> factor( exclude = NULL ) }
  POP <- df[[population]]  |> as.numeric()
  RES <- df[[resource]] |> as.numeric()
  RES <- bottomcode(RES)
  YEAR <- df[[year]] |> factor() 
  
  log_resource <- paste0( "LOG_", resource )
  
    dd1 <-
      data.frame(GEO,SUB,YEAR,POP,RES) %>%
      na.omit() %>%
      dplyr::group_by( GEO, SUB, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        POP2 = mean( POP, na.rm=TRUE ),
                        RES2 = sum( RES, na.rm=TRUE ),
                        # RES2 = ifelse( RES2 > 0, RES2, 0 ),
                        LOG_NPO = log( N + 1 ),
                        LOG_POP = log( POP2 + 1 ),
                        LOG_RES = log( RES2 + 1 ),
                        .groups = "drop" ) 
                        
    dd2 <-
      data.frame(GEO,YEAR,POP,RES) %>%
      na.omit() %>%
      dplyr::group_by( GEO, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        POP2 = mean(POP, na.rm=TRUE),
                        RES2 = sum(RES, na.rm=TRUE),
                        # RES2 = ifelse( RES2 > 0, RES2, 0 ),
                        LOG_NPO = log( N + 1 ),
                        LOG_POP = log( POP2 + 1 ),
                        LOG_RES = log( RES2 + 1 ),
                        .groups = "drop" )
    dd2$SUB <- "ALL"
    
    dd3 <-
      data.frame(SUB,YEAR,POP,RES) %>%
      na.omit() %>%
      dplyr::group_by( SUB, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        POP2 = mean(POP, na.rm=TRUE),
                        RES2 = sum(RES, na.rm=TRUE),
                        # RES2 = ifelse( RES2 > 0, RES2, 0 ),
                        LOG_NPO = log( N + 1 ),
                        LOG_POP = log( POP2 + 1 ),
                        LOG_RES = log( RES2 + 1 ),
                        .groups = "drop" )
    dd3$GEO <- "ALL"
    
    dd <- bind_rows( dd1, dd2, dd3 ) 
    dd <- dplyr::arrange( dd, GEO, SUB, YEAR )
    
  
  #   get_density_pctl <- function(df) {
  #     # Fit model of logged nonprofits on logged population
  #     m <- lm(LOG_NPO ~ LOG_POP, data = df)
  #     # Compute residuals in logged space, then exponentiate and rescale
  #     logged_residuals <- residuals(m)
  #     density_pctl <- exp(logged_residuals) - 1
  #     density_pctl[ density_pctl > 2 ] <- 2
  #     return(density_pctl)
  #   }

  #   df_density <- dd %>%
  #     group_by(SUB, YEAR) %>%
  #     nest() %>%
  #     mutate(
  #       data = map(data, ~ mutate(.x, density_pctl = get_density_pctl(.x)))
  #     ) %>%
  #     unnest(data)

  get_density_pctl_n <- function(df) {
    m <- lm(LOG_NPO ~ LOG_POP, data = df)
    logged_residuals <- residuals(m)
    DENSITY_PCTL_N <- exp(logged_residuals) - 1
    DENSITY_PCTL_N[DENSITY_PCTL_N > 2] <- 2
    return(list(model_n = m, DENSITY_PCTL_N = DENSITY_PCTL_N))
  }
  
  get_density_pctl_r <- function(df) {
    m <- lm(LOG_RES ~ LOG_POP, data = df)
    logged_residuals <- residuals(m)
    DENSITY_PCTL_R <- exp(logged_residuals) - 1
    DENSITY_PCTL_R[DENSITY_PCTL_R > 2] <- 2
    return(list(model_r = m, DENSITY_PCTL_R = DENSITY_PCTL_R))
  }

  df_density <- dd %>%
    group_by(SUB, YEAR) %>%
    nest() %>%
    mutate(
      model_results_n = map(data, get_density_pctl_n),
      model_n = map(model_results_n, "model_n"),                 # extract model objects
      data = map2(data, model_results_n, \(d, res) mutate(d, DENSITY_PCTL_N = res$DENSITY_PCTL_N))
    ) %>%
    mutate(
      model_results_r = map(data, get_density_pctl_r),
      model_r = map(model_results_r, "model_r"),                 # extract model objects
      data = map2(data, model_results_r, \(d, res) mutate(d, DENSITY_PCTL_R = res$DENSITY_PCTL_R))
    ) %>%    
    dplyr::select(-model_results_n,-model_results_r) %>%
    unnest(data) %>%
    dplyr::rename( POP=POP2, {{resource}} := RES2, {{log_resource}} := LOG_RES )
  
  # df_wide <- df_density %>%
  #   ungroup() %>%  # remove any grouped_df structure
  #   mutate(
  #     GEO = as.character(GEO),
  #     SUB = as.character(SUB),
  #     YEAR = as.character(YEAR),
  #     SUB_YEAR = paste(SUB, YEAR, sep = "_")
  #  ) %>%
  #   dplyr::select(GEO, SUB_YEAR, density_pctl) %>%
  #   distinct() %>%  # drop duplicates if any
  #   pivot_wider(
  #     names_from = SUB_YEAR,
  #     values_from = density_pctl,
  #     values_fill = NA  # fill missing GEO–SUB–YEAR combos
  #   )
  #  
  # return( df_wide )
  return( df_density )
}







df_density <- get_scale_free_density( df )

  df_wide <- df_density %>%
    ungroup() %>%  # remove any grouped_df structure
    mutate(
      GEO = as.character(GEO),
      SUB = as.character(SUB),
      YEAR = as.character(YEAR),
      SUB_YEAR = paste(SUB, YEAR, sep = "_")
    ) %>%
    dplyr::select(GEO, SUB_YEAR, density_pctl) %>%
    distinct() %>%  # drop duplicates if any
    pivot_wider(
      names_from = SUB_YEAR,
      values_from = density_pctl,
      values_fill = NA  # fill missing GEO–SUB–YEAR combos
    )
    
    
    
# Select columns ending with _2021
df_2021x <- df_wide %>%
  dplyr::select(ends_with("_2021"))

jplot( df_2021x$HMS_2021, df_2021$HMS_2021, xlab="Residuals from Total Expense Models", ylab="Residuals from NPO Count Models" )
abline( h=c(-1,0,1), col="darkorange", lty=3 )
abline( v=c(-1,0,1), col="darkorange", lty=3 )
pairs( df_2021x, lower.panel=panel.smooth, upper.panel=panel.cor )


# Compute correlation matrix
cor(df_2021, use = "pairwise.complete.obs")


jplot( df_2021$ART_2021, df_2021$EDU_2021 )

npo.art <- dd$NPO_N[ dd$YEAR == 2011 & dd$SUB == "ART" ]
pop.art <- dd$POP2[ dd$YEAR == 2011 & dd$SUB == "ART" ]
res.art <- dd$RES2[ dd$YEAR == 2011 & dd$SUB == "ART" ]
geo.art <- dd$GEO[ dd$YEAR == 2011 & dd$SUB == "ART" ]


jplot( log( pop.art + 1 ), log( npo.art + 1 )  )
jplot( log( pop.art + 1 ), log( res.art + 1 )  )
jplot( log( pop.art + 1 ), df_2021$ART_2021 )

mm <- lm( log(npo.art+1) ~ log(pop.art) )
summary( mm )
abline( mm, col="firebrick" )

mm <- lm( log(res.art+1) ~ log(pop.art) )
summary( mm )
abline( mm, col="firebrick" )


npo.all <- dd$NPO_N[ dd$YEAR == 2021 & dd$SUB == "ALL" ]
pop.all <- dd$POP2[ dd$YEAR == 2021 & dd$SUB == "ALL" ]
res.all <- dd$RES2[ dd$YEAR == 2021 & dd$SUB == "ALL" ]

jplot( log( pop.all + 1 ), log( npo.all + 1 ), 
       xlab="Population (logged)", ylab="Number of NPOs (logged)", main="Log-Log Model of NP Density"  )

jplot( log( pop.all + 1 ), res.all/pop.all, ylim=c(0,20000), xlab="City Size (log pop)", ylab="Per Capita NPO Spending" )

summary( res.all/pop.all )



jplot( log( pop.all + 1 ), log( res.all + 1 ), 
       xlab="Population (logged)", ylab="Total NPO Expenses (logged)", main="Log-Log Model of NP Scale"  )


mm <- lm( log(npo.all+1) ~ log(pop.all) )
summary( mm )
abline( mm, col="firebrick" )


pcap <- dd$RES2 / dd$POP2
jplot( dd$LOG_POP, pcap ) , 
       xlab="Population (logged)", ylab="Number of NPOs (logged)", main="Log-Log Model of NP Density"  )



dft1 <- data.frame( GEO=df_wide$GEO, ART_2021=df_wide$ART_2021 )
dft2 <- data.frame( GEO=geo.art, POP=pop.art, NPO=npo.art )
dft <- merge( dft1, dft2, by="GEO" )

jplot( log( dft$POP + 1 ), dft$ART_2021 )


jplot( df_wide$HMS_2021, df_wide$HMS_2022 )


df_all <- df_wide %>%
  select(starts_with("ALL_"))

pairs( df_all, lower.panel=panel.smooth, upper.panel=panel.cor )



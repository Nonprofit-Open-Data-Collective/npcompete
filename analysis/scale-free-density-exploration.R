
library(MASS)
library(dplyr)
library(tidyr)
library(purrr)


df <- dplyr::filter( df3, TAXYEAR > 2010 & TAXYEAR < 2023 )
df <- dplyr::filter( df, V501C == 3 )



source( "R/scale-free-density.R" )







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



library( dplyr )
library( data.table )

path1 <- "C:/Users/jdlec/Dropbox (Personal)/00 - DATA/SOI/"
fn1 <- "SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED.CSV"

df <- fread( paste0( path1, fn1 ) )

f <- df[["CENSUS_BLOCK_FIPS"]] |> as.character()
table( nchar(f) )

f2 <- stringr::str_pad( f, width = 15, side = "left", pad = "0" )
table( nchar(f2) )

df$COUNTY_FIPS <- substr( f2, 1, 5 )


path2 <- "C:/Users/jdlec/Dropbox (Personal)/00 - DATA/population/"

fn2 <- "cbsa_population_2000_2025.csv"
fn3 <- "county_population_2000_2025.csv"

pop.c <- read.csv( paste0( path2, fn3 ), colClasses="character" )

pop.c <-
  rename( pop.c,
          COUNTY_FIPS = GEOID,
          COUNTY = NAME, 
          YEAR = year, 
          POP_COUNTY = population ) %>%
  select( COUNTY_FIPS, COUNTY, YEAR, POP_COUNTY )
 
pop.m  <- read.csv( paste0( path2, fn2 ), colClasses="character" )

pop.m <- 
  rename( pop.m,
          CBSA_FIPS = cbsacode,
          CBSA = cbsatitle, 
          YEAR = year, 
          POP_METRO = population ) %>%
  select( CBSA_FIPS, CBSA, YEAR, POP_METRO )

df$TAXYEAR <- as.character( df$TAXYEAR )
df$CENSUS_CBSA_FIPS <- as.character( df$CENSUS_CBSA_FIPS )

df2 <- 
  merge( df, pop.c, 
         by.x=c("COUNTY_FIPS","TAXYEAR"), 
         by.y=c("COUNTY_FIPS","YEAR"),
         all.x=TRUE )


dim( df )
dim( df2 )

pop.m <- na.omit( pop.m )

df3 <- 
  merge( df2, pop.m, 
         by.x=c("CENSUS_CBSA_FIPS","TAXYEAR"), 
         by.y=c("CBSA_FIPS","YEAR"),
         all.x=TRUE )

dim( df3 )

df3 <- arrange( df3, EIN2, TAXYEAR )
head( df3 )

# FIX NTEE

path <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/nccstools/"
source( paste0( path, "nteev2.R" ) )

v1 <- df3[["NTEE_NCCS"]]
v2 <- df3[["NTEE_IRS"]]
v1[ v1 == "" | is.na(v1) ] <- v2[ v1 == "" | is.na(v1) ]
sum( v1 == "" )

v3 <- get_nteev2( v1 )

df3$NTEEV2 <- v3

fwrite( df3, "SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED-W-POP.CSV" )





#####################
#####################




ntee <- 
  df3 %>%
  filter( ! ( is.na(NTEEV2) | NTEEV2 == "" ) ) %>%
  sample_n( 10000 ) %>%
  select( NTEE_IRS, NTEE_NCCS, NTEEV2 )

v1 <- ntee[["NTEE_NCCS"]]
v2 <- get_nteev2( v1 )
sum( v2 != ntee[["NTEEV2"]] )

ntee$V2 <- v2
ntee[ v2 != ntee[["NTEEV2"]] , c("NTEE_IRS","NTEE_NCCS","NTEEV2","V2") ] |> knitr::kable()




gini <- get_gini( df3 )
head( gini, 200 ) |> as.data.frame()



gini2 <- filter( gini, N > 1 & YEAR %in% 2011:2022 )

plot( gini2$N, gini2$GINI, pch=19, col=gray(0.5,0.5) )

t <- tapply( gini2$GINI, gini2$N, median )

plot( log10(as.numeric(names(t))), t, pch=19, col=gray(0.5,0.5),
      xlab="Number of Nonprofits (log10)", ylab="Gini Coefficient",
      bty="n", xlim=c(0.5,4) )

gini2$YEAR <- factor( gini2$YEAR )
t <- tapply( gini2$GINI, gini2$YEAR, median, na.rm=T )

plot( as.numeric(names(t)), t, pch=19, type="b",
      xlab="Year", ylab="Gini Coefficient",
      bty="n", cex=2 )


hhi <- get_hhi( df3 )

hhi2 <- filter( hhi, N > 1 & YEAR %in% 2011:2022 )
t <- tapply( hhi2$HHI, hhi2$N, median )

plot( log10(as.numeric(names(t))), t, pch=19, col=gray(0.5,0.5),
      xlab="Number of Nonprofits (log10)", ylab="HHI Index",
      bty="n" )
hhi2$YEAR <- factor( hhi2$YEAR )
t <- tapply( hhi2$HHI, hhi2$YEAR, median, na.rm=T )
plot( as.numeric(names(t)), t, pch=19, type="b",
      xlab="Year", ylab="HHI Index",
      bty="n", cex=2 )


nhhi <- get_nhhi( df3 )


d2 <- filter( nhhi, N > 1 & YEAR %in% 2011:2022 )
t1 <- tapply( d2$HHI, d2$N, median )
t2 <- tapply( d2$NHHI, d2$N, median )

plot( log10(as.numeric(names(t1))), t1, pch=19, col="steelblue",
      xlab="Number of Nonprofits (log10)", ylab="HHI Index",
      bty="n", main="NHHI (red) vs HHI (blue)" )
points( log10(as.numeric(names(t2))), t2, pch=19, col="firebrick" )

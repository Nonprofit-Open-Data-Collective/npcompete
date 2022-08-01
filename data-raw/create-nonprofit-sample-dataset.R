###-----------------------
###Script to create the nonprofit-sample dataset
###-----------------------

library( haven )        # importing data files
library( tidyr )        # data wrangling
library( dplyr )        # data wrangling
library( ggplot2 )      # fancy plots
library( ggthemes )     # fancy plots
library( scales )       # re-scaling numbers
library( stargazer )    # nice tables
library( pander )       # format tables for HTML
library( DT )           # embed datasets in HTML docs 

core.2019 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/coreco.core2019pc.csv")
core.2018 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/coreco.core2018pc.csv")
core.2017 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/coreco.core2017pc.csv")
core.2016 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/coreco.core2016pc.csv")
core.2015 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/nccs.core2015pc.csv")

core.2019$year <- 2019
core.2018$year <- 2018
core.2017$year <- 2017
core.2016$year <- 2016
core.2015$year <- 2015

core.2019 <- data.frame(lapply(core.2019, as.character), stringsAsFactors=FALSE)
core.2018 <- data.frame(lapply(core.2018, as.character), stringsAsFactors=FALSE)
core.2017 <- data.frame(lapply(core.2017, as.character), stringsAsFactors=FALSE)
core.2016 <- data.frame(lapply(core.2016, as.character), stringsAsFactors=FALSE)
core.2015 <- data.frame(lapply(core.2015, as.character), stringsAsFactors=FALSE)

d.s <- get_summary(sample.dat, 'MSA_NECH', 'NTMAJ12')
sample.dat <- bind_rows(core.2015, core.2016, core.2017, core.2018, core.2019)

d.s <- d.s %>%
  drop_na(geo)

get_summary <- function(df, geo, subsector){
  
  df <- df %>% rename(geo = {{geo}},
                      subsector = {{subsector}})
  
  dat.sum <- df %>% dplyr::mutate( geo=factor(geo),
                 subsector=factor(subsector)) %>%
    dplyr::group_by( geo, subsector ) %>%
    dplyr::summarize( n=dplyr::n() )
  return (dat.sum)
}


#After sorting the combined dataframe by n, 5 MSA's were randomly chosen with 
#2 MSA's and subsector's having largest n, 2 with low n and 1 around the middle value. 




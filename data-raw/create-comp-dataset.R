###-----------------------
###Script to create the competition dataset npcompete
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

source('R/hhi.R')
source('R/cr.R')
source('R/kwoka-index.R')
source('R/density.R')
source('R/gini.R')
source('R/fundraising-efficiency.R')
source('R/all-metrics.R')


core.2019 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/coreco.core2019pc.csv")
core.2018 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/coreco.core2018pc.csv")
core.2017 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/coreco.core2017pc.csv")
core.2016 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/coreco.core2016pc.csv")
core.2015 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/nccs.core2015pc.csv")
core.2014 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/nccs.core2014pc.csv")
core.2013 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/nccs.core2013pc.csv")
core.2012 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/nccs.core2012pc.csv")
core.2011 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/nccs.core2011pc.csv")
core.2010 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2010-2019/nccs.core2010pc.csv")
core.2009 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2000-2009/nccs.core2009pc.csv")
core.2008 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2000-2009/nccs.core2008pc.csv")
core.2007 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2000-2009/nccs.core2007pc.csv")
core.2006 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2000-2009/nccs.core2006pc.csv")
core.2005 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2000-2009/nccs.core2005pc.csv")
core.2004 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2000-2009/nccs.core2004pc.csv")
core.2003 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2000-2009/nccs.core2003pc.csv")
core.2002 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2000-2009/nccs.core2002pc.csv")
core.2001 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2000-2009/nccs.core2001pc.csv")
core.2000 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/2000-2009/nccs.core2000pc.csv")
core.1999 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/1990-1999/nccs.core1999pc.csv")
core.1998 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/1990-1999/nccs.core1998pc.csv")
core.1997 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/1990-1999/nccs.core1997pc.csv")
core.1996 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/1990-1999/nccs.core1996pc.csv")
core.1995 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/1990-1999/nccs.core1995pc.csv")
core.1994 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/1990-1999/nccs.core1994pc.csv")
core.1993 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/1990-1999/nccs.core1993pc.csv")
core.1992 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/1990-1999/nccs.core1992pc.csv")
core.1991 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/1990-1999/nccs.core1991pc.csv")
core.1990 <- read.csv("C:/Users/pradh/Desktop/Urban/Test_Data/nccs-core/1990-1999/nccs.core1990pc.csv")

core.2019$year <- 2019
core.2018$year <- 2018
core.2017$year <- 2017
core.2016$year <- 2016
core.2015$year <- 2015
core.2014$year <- 2014
core.2013$year <- 2013
core.2012$year <- 2012
core.2011$year <- 2011
core.2010$year <- 2010
core.2009$year <- 2009
core.2008$year <- 2008
core.2007$year <- 2007
core.2006$year <- 2006
core.2005$year <- 2005
core.2004$year <- 2004
core.2003$year <- 2003
core.2002$year <- 2002
core.2001$year <- 2001
core.2000$year <- 2000
core.1999$year <- 1999
core.1998$year <- 1998
core.1997$year <- 1997
core.1996$year <- 1996
core.1995$year <- 1995
core.1994$year <- 1994
core.1993$year <- 1993
core.1992$year <- 1992
core.1991$year <- 1991
core.1990$year <- 1990



core.files <- list(core.2019,core.2018,core.2017,core.2016,core.2015,core.2014,core.2013,
          core.2012,core.2011,core.2010,core.2009,core.2008,core.2007,core.2006,core.2005,core.2004,
          core.2003,core.2002,core.2001,core.2000,core.1999,core.1998,core.1997,core.1996,core.1995,core.1994,
          core.1993,core.1992,core.1991,core.1990)

 
den.col.names <- c('PROGREV', 'INVINC')
fund.col.names <- c('FUNDFEES', 'SOLICIT')
indexlist = list()

i = 1
for (cf in core.files){
  
  cfile <- as.data.frame(cf)
  names( cfile ) <- toupper( names( cfile ))
  cfile <- select( cfile, EIN, MSA_NECH, NTMAJ12, TOTREV, CONT, FIPS, PROGREV,INVINC ,YEAR)
  
  
  cfile$NTMAJ12[ cfile$NTMAJ12 == "AR" ] <- "Arts"
  cfile$NTMAJ12[ cfile$NTMAJ12 == "BH" ] <- "Universities"
  cfile$NTMAJ12[ cfile$NTMAJ12 == "ED" ] <- "Education"
  cfile$NTMAJ12[ cfile$NTMAJ12 == "EH" ] <- "Hospitals"
  cfile$NTMAJ12[ cfile$NTMAJ12 == "EN" ] <- "Environmental"
  cfile$NTMAJ12[ cfile$NTMAJ12 == "HE" ] <- "Health"
  cfile$NTMAJ12[ cfile$NTMAJ12 == "HU" ] <- "Human Services"
  cfile$NTMAJ12[ cfile$NTMAJ12 == "IN" ] <- "International"
  cfile$NTMAJ12[ cfile$NTMAJ12 == "MU" ] <- "Mutual Benefit"
  cfile$NTMAJ12[ cfile$NTMAJ12 == "PU" ] <- "Public Benefit"
  cfile$NTMAJ12[ cfile$NTMAJ12 == "RE" ] <- "Religion"
  cfile$NTMAJ12[ cfile$NTMAJ12 == "UN" ] <- "Unknown"

  y <- cfile[2,'YEAR']
  temp <- get_all_metrics(cfile,'MSA_NECH','NTMAJ12','TOTREV', 'Revenue', 'EIN', 'FIPS', y , 100000, 1000000, fund.col.names, den.col.names)
  indexlist[[i]] <- temp 
  i <- i+1
}
dat.allyears <- bind_rows(indexlist)
dat.allyears <- dat.allyears %>%
                drop_na(geo)

save(dat.allyears, file="competition-dataset.rda")



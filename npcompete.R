# Load Packages
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
source('R/cr4.R')
source('R/kwoka-index.R')

#Read the core data file(.csv)
core.2019 <- read.csv( "Data/coreco.core2019pc.csv" )

#Convert all names to upper case
names( core.2019 ) <- toupper( names( core.2019 ))

core.data <- select( core.2019, EIN, MSA_NECH, NTMAJ12, TOTREV, CONT )

core.data$NTMAJ12[ core.data$NTMAJ12 == "AR" ] <- "Arts"
core.data$NTMAJ12[ core.data$NTMAJ12 == "BH" ] <- "Universities"
core.data$NTMAJ12[ core.data$NTMAJ12 == "ED" ] <- "Education"
core.data$NTMAJ12[ core.data$NTMAJ12 == "EH" ] <- "Hospitals"
core.data$NTMAJ12[ core.data$NTMAJ12 == "EN" ] <- "Environmental"
core.data$NTMAJ12[ core.data$NTMAJ12 == "HE" ] <- "Health"
core.data$NTMAJ12[ core.data$NTMAJ12 == "HU" ] <- "Human Services"
core.data$NTMAJ12[ core.data$NTMAJ12 == "IN" ] <- "International"
core.data$NTMAJ12[ core.data$NTMAJ12 == "MU" ] <- "Mutual Benefit"
core.data$NTMAJ12[ core.data$NTMAJ12 == "PU" ] <- "Public Benefit"
core.data$NTMAJ12[ core.data$NTMAJ12 == "RE" ] <- "Religion"
core.data$NTMAJ12[ core.data$NTMAJ12 == "UN" ] <- "Unknown"

core.data$NTMAJ12 <- factor( core.data$NTMAJ12 )

#Recode negative revenues as zero because they cause HHIs above 1:
core.data$TOTREV3 <- core.data$TOTREV
core.data$TOTREV[ core.data$TOTREV < 0 ] <- 0

dat.hhi <- get_hhi(core.data,'MSA_NECH','NTMAJ12','TOTREV', 'CONT')
dat.nhhi <- get_nhhi(core.data,'MSA_NECH','NTMAJ12','TOTREV', 'CONT')

#dat.nhhi %>%
#  group_by( NTMAJ12 ) %>%
#  summarize( n=n(), min=min(nhhi), max=max(nhhi) ) %>%
#  pander()

dat.cr4 <- get_cr4(core.data,'MSA_NECH','NTMAJ12','TOTREV')
dat.cr2 <- get_cr2(core.data,'MSA_NECH','NTMAJ12','TOTREV')

dat.kindex <- get_kwoka_index(core.data,'MSA_NECH','NTMAJ12','TOTREV')



###-----------------------
###Script to create the county population dataset
###-----------------------
library(censusapi)
censuskey <- "512b93a7e59497dd0360c30b65a3ca147a29ccfa"
apis <- listCensusApis()
View(apis)

##################2000-2010 using pep.int population#####################
pop.all <- getCensus( name="pep/int_population",
                      vintage=2000,
                      key=censuskey,
                      vars=c("POP","DATE_DESC"),
                      region="county:*")

pop.2000 <- spread(pop.all, DATE_DESC, POP)
drop.cols <- c("4/1/2000 population estimates base","4/1/2010 Census 2010 population")
pop.2000 <- pop.2000[ , !(names(pop.2000) %in% drop.cols)]
pop.2000 <- pop.2000 %>% 
            rename(POPESTIMATE2000='7/1/2000 population estimate',
                   POPESTIMATE2001='7/1/2001 population estimate',
                   POPESTIMATE2002='7/1/2002 population estimate',
                   POPESTIMATE2003='7/1/2003 population estimate',
                   POPESTIMATE2004='7/1/2004 population estimate',
                   POPESTIMATE2005='7/1/2005 population estimate',
                   POPESTIMATE2006='7/1/2006 population estimate',
                   POPESTIMATE2007='7/1/2007 population estimate',
                   POPESTIMATE2008='7/1/2008 population estimate',
                   POPESTIMATE2009='7/1/2009 population estimate')



pop.2000$FIPS <- as.numeric( paste0( pop.2000$state, pop.2000$county ))
pop.2000 <- pop.2000 %>% mutate(FIPS = sprintf("%05d", FIPS))
#pop.2000 <- subset(pop.2000, STATE != '72')#removing puerto rico

##############2010-2019 using acs#############################

pop.latest <- getCensus( name="acs/acs5",
                       vintage=2011,
                       key=censuskey,
                       vars=c("B01001_001E"),
                       region="county:*")
pop.latest$Date <- 2011

for (year in 2012:2020){
  pop.temp <- getCensus( name="acs/acs5",
                        vintage=all_of(year),
                        key=censuskey,
                        vars=c("B01001_001E"),
                        region="county:*")
  pop.temp$Date <- all_of(year)
  pop.latest <- rbind(pop.latest, pop.temp)
  
}

pop.2020 <- spread(pop.latest, Date, B01001_001E)
pop.2020$FIPS <- as.numeric( paste0( pop.2020$state, pop.2020$county ))
pop.2020 <- pop.2020 %>% 
  rename(POPESTIMATE2010 ='2010',
         POPESTIMATE2011 ='2011',
         POPESTIMATE2012 ='2012',
         POPESTIMATE2013 ='2013',
         POPESTIMATE2014 ='2014',
         POPESTIMATE2015 ='2015',
         POPESTIMATE2016 ='2016',
         POPESTIMATE2017 ='2017',
         POPESTIMATE2018 ='2018',
         POPESTIMATE2019 ='2019',
         POPESTIMATE2020 ='2020')
pop.2020 <- pop.2020 %>% mutate(FIPS = sprintf("%05d", FIPS))

#############################################################
pop.data <- merge( pop.2000, pop.2020, by.x="FIPS", by.y="FIPS", all.x=T )
drop.cols <- c('county.x', 'state.x','county.y', 'state.y')
pop.data <- pop.data[ , !(names(pop.data) %in% drop.cols)]

###################1990-1999(read from file)###################################

pop.1999 <- read.csv( "Test_Data/population/county_population.csv" )
pop.1990 <- pop.1999[!(pop.1999$county_fips > 1000),]
pop.1990 <- select(pop.1990, fips, pop1990, pop1991, pop1992, pop1993, pop1994, pop1995, pop1996, pop1997,
                   pop1998, pop1999)

pop.1990 <- pop.1990 %>% rename(FIPS = fips) %>% mutate(FIPS = sprintf("%05d", FIPS))
pop.1990 <- pop.1990 %>% 
  rename(POPESTIMATE1990 ='pop1990',
         POPESTIMATE1991 ='pop1991',
         POPESTIMATE1992 ='pop1992',
         POPESTIMATE1993 ='pop1993',
         POPESTIMATE1994 ='pop1994',
         POPESTIMATE1995 ='pop1995',
         POPESTIMATE1996 ='pop1996',
         POPESTIMATE1997 ='pop1997',
         POPESTIMATE1998 ='pop1998',
         POPESTIMATE1999 ='pop1999')

pop.data <- merge( pop.data, pop.1990, by.x="FIPS", by.y="FIPS", all.x=T )
save(pop.data, file="data/county_population.rda")




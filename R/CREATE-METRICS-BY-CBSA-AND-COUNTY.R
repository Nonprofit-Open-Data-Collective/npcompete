#' Compute all market concentration metrics and return a single data frame
#'
#' Combines Gini, normalized HHI, Kwoka, and concentration ratios (CR1–CR8)
#' into a unified summary for each (GEO, SUB, YEAR) group.
#'
#' @param df Data frame containing market data.
#' @param geography Column name identifying the geography field.
#' @param subsector Column name identifying the subsector field.
#' @param resource Column name identifying the resource or revenue variable.
#' @param year Column name identifying the year field.
#' @param numbers Numeric vector of CR levels to compute (default = 1:8).
#'
#' @return A merged tibble with columns:
#'   GEO, SUB, YEAR, N, TOTREV, GINI, HHI, NHHI, KWOKA, CR1–CR8
#'
#' @export
get_all_metrics <- function(df,
                                  geography = "CENSUS_CBSA_FIPS",
                                  population = "POP_METRO",
                                  subsector = "NTEEV2",
                                  resource  = "TOTREV",
                                  year      = "TAXYEAR",
                                  cr_n      = 1:8) {

  # --- Run all individual metrics ------------------------------------------
  message("Calculating Gini...")
  dat_gini  <- get_gini(df, geography, subsector, resource, year)

  message("Calculating normalized HHI...")
  dat_nhhi  <- get_nhhi(df, geography, subsector, resource, year)

  message("Calculating Kwoka index...")
  dat_kwoka <- get_kwoka(df, geography, subsector, resource, year)

  message("Calculating concentration ratios...")
  dat_cr    <- get_cr_all(df, geography, subsector, year, resource, numbers=cr_n)

  message("Calculating nonprofit density...")
  dat_den   <- get_scale_free_density(df, geography, population, subsector, year, resource)
  dat_den   <- dplyr::select( dat_den, -model_n, -model_r, -N, -{{resource}} ) 

  # --- Standardize join keys ----------------------------------------------
  by_keys <- c("GEO", "SUB", "YEAR")

  # --- Merge all results ---------------------------------------------------
  out <- dat_gini %>%
    dplyr::full_join( dat_nhhi ) %>%
    dplyr::full_join( dat_kwoka ) %>%
    dplyr::full_join( dat_cr ) %>%
    dplyr::full_join( dat_den, by = by_keys ) 

  # --- Tidy up column order ------------------------------------------------
  log_resource <- paste0( "LOG_", resource )
  metric_cols <- c(
    "N", "POP", resource, "LOG_NPO", "LOG_POP", log_resource, 
    "DENSITY_PCTL_N", "DENSITY_PCTL_R", "GINI", "HHI", "NHHI", "KWOKA",
    paste0("CR", cr_n)
  )
  existing_cols <- intersect(metric_cols, names(out))
  out <- dplyr::select(out, GEO, SUB, YEAR, dplyr::all_of(existing_cols))

  return(out)
}


df1 <- fread( "SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED-W-POP.CSV" )
df <- dplyr::filter( df1, TAXYEAR > 2010 & TAXYEAR < 2023 )
df <- dplyr::filter( df, V501C == 3 )



all <- 
  get_all_metrics( 
    df, resource="TOTREV", 
    geography="CENSUS_CBSA_FIPS", 
    population="POP_METRO" )


-----------------------------------------------------------------------------
  GEO    SUB   YEAR   N     POP     TOTREV    LOG_NPO   LOG_POP   LOG_TOTREV 
------- ----- ------ ---- ------- ---------- --------- --------- ------------
 01001   ALL   2011   38   54994   15964703    3.664     10.92      16.59    

 01001   ALL   2012   40   55418   16719009    3.714     10.92      16.63    

 01001   ALL   2013   40   55841   25401247    3.714     10.93      17.05    

 01001   ALL   2014   41   56265   18941766    3.738     10.94      16.76    

 01001   ALL   2015   44   56688   18304363    3.807     10.95      16.72    

 01001   ALL   2016   46   57111   18923631    3.85      10.95      16.76    
-----------------------------------------------------------------------------

Table: Table continues below

 
--------------------------------------------------------------------------------
 DENSITY_PCTL_N   DENSITY_PCTL_R    GINI      HHI      NHHI     KWOKA     CR1   
---------------- ---------------- -------- --------- --------- -------- --------
    -0.2782          -0.7217       0.6516   0.09977   0.07544   0.3627   0.2294 

    -0.2454          -0.7207       0.665    0.09986   0.07678   0.3644   0.2311 

    -0.2527          -0.5857       0.7702   0.1738    0.1527    0.4318   0.3578 

    -0.2444          -0.7082       0.6902   0.1031    0.08072   0.3722   0.2366 

    -0.2058          -0.7249       0.7024   0.1006    0.07971   0.3627   0.2294 

    -0.1764          -0.7306       0.7451   0.1092    0.08937   0.3686   0.2354 
--------------------------------------------------------------------------------

Table: Table continues below

 
--------------------------------------------------------------
  CR2      CR3      CR4      CR5      CR6      CR7      CR8   
-------- -------- -------- -------- -------- -------- --------
 0.3608   0.4851   0.5475   0.5952   0.641    0.6745   0.7058 

 0.3646   0.4851   0.5483   0.5945   0.6321   0.6675   0.7014 

 0.5144   0.6131   0.6925   0.7327   0.7617   0.7859   0.806  

 0.3654   0.4796   0.5671    0.62    0.6577   0.6896   0.7163 

 0.3657   0.486    0.5425   0.5946   0.6402   0.6851    0.72  

 0.3674   0.4847   0.5994   0.6539   0.6921   0.7277   0.7554 
--------------------------------------------------------------


####
####   METROS
####

all_m_totrev <- 
  get_all_metrics( 
    df, resource="TOTREV", 
    geography="CENSUS_CBSA_FIPS", 
    population="POP_METRO" )

all_m_totexp <- 
  get_all_metrics( 
    df, resource="TOTEXP", 
    geography="CENSUS_CBSA_FIPS", 
    population="POP_METRO" )

all_m_netass <- 
  get_all_metrics( 
    df, resource="NETASS", 
    geography="CENSUS_CBSA_FIPS", 
    population="POP_METRO" )

data.table::fwrite( all_m_totrev, "CONCENTRATION-METRICS-BY-CBSA-TOTREV.CSV" )
data.table::fwrite( all_m_totexp, "CONCENTRATION-METRICS-BY-CBSA-TOTEXP.CSV" )
data.table::fwrite( all_m_netass, "CONCENTRATION-METRICS-BY-CBSA-NETASS.CSV" )


####
####   COUNTIES 
####

all_m_totrev <- 
  get_all_metrics( 
    df, resource="TOTREV", 
    geography="COUNTY_FIPS", 
    population="POP_COUNTY" )

all_m_totexp <- 
  get_all_metrics( 
    df, resource="TOTEXP", 
    geography="COUNTY_FIPS", 
    population="POP_COUNTY" )

all_m_netass <- 
  get_all_metrics( 
    df, resource="NETASS", 
    geography="COUNTY_FIPS", 
    population="POP_COUNTY" )

data.table::fwrite( all_m_totrev, "CONCENTRATION-METRICS-BY-COUNTY-TOTREV.CSV" )
data.table::fwrite( all_m_totexp, "CONCENTRATION-METRICS-BY-COUNTY-TOTEXP.CSV" )
data.table::fwrite( all_m_netass, "CONCENTRATION-METRICS-BY-COUNTY-NETASS.CSV" )






# make sure no duplicate GEO ids from merge

all_m_totrev %>% 
  arrange( GEO, YEAR, SUB ) %>%
  group_by( GEO, SUB, YEAR ) %>% 
  mutate( count=n() ) %>% 
  dplyr::filter( count > 1 )

------------------------------------------------------------------------------
 CENSUS_CBSA_FIPS   TAXYEAR   COUNTY_FIPS        EIN2             ROWID       
------------------ --------- ------------- ---------------- ------------------
      14460          2013        25009      EIN-00-1070499   95139eeaf1b39479 

      14460          2015        25009      EIN-01-0011694   1fbdb37621e20291 

      14460          2016        25009      EIN-01-0011694   576696b9956c3229 

      14460          2017        25009      EIN-01-0011694   c181a609b143868c 

      14460          2018        25009      EIN-01-0011694   2ca59b407a8072bb 

      14460          2019        25009      EIN-01-0011694   3c82871967e24816 
------------------------------------------------------------------------------

Table: Table continues below

 
------------------------------------------------------------------------
 V501C   EFILE_X   TOTEXP   TOTREV   TOTASS   TOTLIA   NETASS   REVPROG 
------- --------- -------- -------- -------- -------- -------- ---------
   7       UNK      4080     2530    16722      0      16722     1250   

   3      PAPER     6528    10513    29135      0      29135     4677   

   3      PAPER     8505     7044    27674      0      27674     3089   

   3      PAPER     6813     8273    29134      0      29134     4041   

   3      PAPER     6292     8427    31269      0      31269     3182   

   3      PAPER     6410     8332    33191      0      33191     2795   
------------------------------------------------------------------------

Table: Table continues below

 
------------------------------------------------------------------------------
 REVCONT   REVINVE   FNDFEE   TAXPEND   FISYEAR   FISMO     SOURCE      LTD_X 
--------- --------- -------- --------- --------- ------- ------------- -------
    0         0        NA     201312     2013      12     SOI_2014_EZ         

    0        36        NA     201609     2016       9     SOI_2022_EZ         

    0        18        NA     201709     2017       9     SOI_2022_EZ         

    0        18        NA     201809     2018       9     SOI_2022_EZ         

    0        19        NA     201909     2019       9     SOI_2022_EZ         

    0        20        NA     202009     2020       9     SOI_2022_EZ         
------------------------------------------------------------------------------

Table: Table continues below

 
----------------------------------------------------
 DOA_X   LTD_DOA_X   MISSING_YEAR   revocation_date 
------- ----------- -------------- -----------------
             N            0                         

             N            0                         

             N            0                         

             N            0                         

             N            0           15-FEB-2019   

             N            0                         
----------------------------------------------------

Table: Table continues below

 
----------------------------------------------------------------------------
 exemption_reinstatement_date   REVOKED_YEAR_X   ORG_RULING_YEAR   NTEE_IRS 
------------------------------ ---------------- ----------------- ----------
                                      0               2013           N67    

                                      0               2021           S19    

                                      0               2021           S19    

                                      0               2021           S19    

         15-FEB-2019                  1               2021           S19    

                                      0               2021           S19    
----------------------------------------------------------------------------

Table: Table continues below

 
-------------------------------------------------------------------
 NTEE_NCCS     NTEEV2     CENSUS_BLOCK_FIPS   LATITUDE   LONGITUDE 
----------- ------------ ------------------- ---------- -----------
    N67      HMS-N67-RG       2.501e+14        42.46       -70.9   

    S19      PSB-S00-NS       2.501e+14        42.77      -71.08   

    S19      PSB-S00-NS       2.501e+14        42.77      -71.08   

    S19      PSB-S00-NS       2.501e+14        42.77      -71.08   

    S19      PSB-S00-NS       2.501e+14        42.77      -71.08   

    S19      PSB-S00-NS       2.501e+14        42.77      -71.08   
-------------------------------------------------------------------

Table: Table continues below

 
--------------------------------------------------------------------------
 BMF_SUBSECTION_CODE         ORG_NAME_CURRENT         COUNTY   POP_COUNTY 
--------------------- ------------------------------ -------- ------------
          7            MARBLEHEAD INTERNATIONAL ONE   Essex      763160   
                             DESIGN FLEET INC                             

          3              MASSACHUSETTS MODERATORS     Essex      776494   
                               ASSOCIATION                                

          3              MASSACHUSETTS MODERATORS     Essex      783161   
                               ASSOCIATION                                

          3              MASSACHUSETTS MODERATORS     Essex      789828   
                               ASSOCIATION                                

          3              MASSACHUSETTS MODERATORS     Essex      796495   
                               ASSOCIATION                                

          3              MASSACHUSETTS MODERATORS     Essex      803162   
                               ASSOCIATION                                
--------------------------------------------------------------------------

Table: Table continues below

 
-------------------------------------------------------------------
              CBSA                POP_METRO         NTMAJ12        
-------------------------------- ----------- ----------------------
 Boston-Cambridge-Newton, MA-NH    4669171    HUMAN SERVICES {HMS} 

 Boston-Cambridge-Newton, MA-NH    4747017    PUBLIC BENEFIT {PSB} 

 Boston-Cambridge-Newton, MA-NH    4785940    PUBLIC BENEFIT {PSB} 

 Boston-Cambridge-Newton, MA-NH    4824863    PUBLIC BENEFIT {PSB} 

 Boston-Cambridge-Newton, MA-NH    4863786    PUBLIC BENEFIT {PSB} 

 Boston-Cambridge-Newton, MA-NH    4902709    PUBLIC BENEFIT {PSB} 
-------------------------------------------------------------------

# AFSC.GAP.DBE R Package (formerly design-based-indices code repository. I'm not tied to the name of the package)

Code to generate design-based indices of abundance from survey data. The code takes haul and catch tables from RACEBASE to estimate biomass for (currently) the GOA, AI, EBS Shelf, and NBS regions. @MargaretSiple-NOAA created a [vignette](https://github.com/zoyafuso-NOAA/design-based-indices/tree/master/old_scripts/vignettes) on the biomass calculations using equations (from [Wakabayashi et al. 1985](https://drive.google.com/file/d/1m5c1N4WYysM1pscrpcgWOGZSZIK8vIHr/view?usp=sharing). We should make this into a wiki and have different pages for the different calculations (biomass, age compositions, etc.) along with other relevant information. 

## Authors

Primary contacts: @MargaretSiple-NOAA, @zoyafuso-NOAA

Groundfish Assessment Program
Resource Assessment and Conservation Engineering Division
Alaska Fisheries Science Center
National Marine Fisheries Service
National Oceanic and Atmospheric Administration

# Initial Testing

Make sure you have installed R packages devtools, RODBC, and getPass and are connected to the VPN when using this package. More instructions on how to install RODBC...? 

```
library(devtools)
devtools::install_github("zoyafuso-NOAA/design-based-indices")

library(AFSC.GAP.DBE)

## Connect to Oracle. See ?AFSC.GAP.DBE::get_connected first for more details
sql_channel <- AFSC.GAP.DBE::get_connected()

## Pull data.  See ?AFSC.GAP.DBE::get_data first for more details
test_data <- AFSC.GAP.DBE::get_data( 
  year_set = 2021,
  survey_set = "EBS_SHELF",
  spp_codes = data.frame(GROUP = c(21720), 
                         SPECIES_CODE = c(21720)),   
  haul_type = 3,
  abundance_haul = "Y",
  pull_lengths = TRUE,
  sql_channel = sql_channel)

## Fill in zeros and calculate CPUE. See ?AFSC.GAP.DBE::calc_cpue first for more details
test_cpue <- AFSC.GAP.DBE::calc_cpue(racebase_tables = test_data)

## Calculate biomass, population abundance, and CIs for each strata. See ?AFSC.GAP.DBE::calc_biomass_stratum first for more details
test_biomass_stratum <- 
  AFSC.GAP.DBE::calc_biomass_stratum(racebase_tables = test_data,
                                     cpue = test_cpue,
                                     vulnerability = 1)

## Calculate aggregated biomass and population abundance across subareas and region (STRATUM 999). See ?AFSC.GAP.DBE::calc_agg_biomass first for more details
test_biomass <- 
  AFSC.GAP.DBE::calc_agg_biomass(biomass_strata = test_biomass_stratum,
                                region = "EBS_STANDARD")
test_biomass2 <- 
  AFSC.GAP.DBE::calc_agg_biomass(biomass_strata = test_biomass_stratum, 
                                region = "EBS_PLUSNW")

## Calculate size composition by stratum. See ?AFSC.GAP.DBE::calc_size_stratum_BS first for more details. Note if you are calculating size comps for the AI/GOA regions, you should be using AFSC.GAP.DBE::calc_size_stratum_AIGOA instead. 
test_size_comp <- AFSC.GAP.DBE::calc_size_stratum_BS(
  racebase_tables = test_data,
  racebase_cpue = test_cpue,
  racebase_stratum_popn = test_biomass_stratum
)

## Calculate age composition for region. See ?AFSC.GAP.DBE::calc_age_comp first for more details. Note
test_age_comp <- 
  AFSC.GAP.DBE::calc_age_comp(
    racebase_tables = test_data, 
    size_comp = test_size_comp
  )

```

## Questions for the future of GAP index products

1. How do we reconcile differences in how the CIs (specifically the degrees of freedom) are calculated for both biomass and population abundance?
2. Do we want, in the future, to provide stock assessment authors with CPUE tables only? Or do we want to continue providing biomass indices.
3. All three regions have different methods for producing size and maybe age comps -- is it worth including scripts for three different methods? Different versions probably already exist in various locations.
4. How do we incorporate code testing to (a) compare these output with the tables in Oracle and (b) to make sure changes in the code don't change estimates unintentionally. 


## Legal disclaimer
This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.

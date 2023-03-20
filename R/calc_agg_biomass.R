#' Calculate index of total biomass across aggregated subareas
#'
#' @param biomass_strata a dataframe of stratum biomass, result object from 
#'                       `AFSC.GAP.DBE::calc_biomass_stratum()`
#'
#' @return dataframe of biomass and population abundance estimates across 
#'         subareas and across the region, along with variances.
#' @export
#' 

calc_agg_biomass <- function(biomass_strata = NULL) {
  
  ## Which survey designs to pull from
  subarea_biomass <- data.frame()
  survey_designs <- 
    biomass_strata[, c("SURVEY", "DESIGN_YEAR")][
      !duplicated(biomass_strata[, c("SURVEY", "DESIGN_YEAR")]) ,
      ]
  
  for (isurvey in 1:nrow(survey_designs)) {
    
    subareas <- subset(x = AFSC.GAP.DBE::new_subarea_descriptions,
                       subset = SURVEY == survey_designs$SURVEY[isurvey] & 
                         DESIGN_YEAR == survey_designs$DESIGN_YEAR[isurvey])
    
    for (isubarea in 1:nrow(subareas)) {
      strata_in_subarea <- 
        subset(x = AFSC.GAP.DBE::new_stratum_groupings,
               subset = AREA_ID %in% subareas$AREA_ID[isubarea])
      
      if (nrow(strata_in_subarea) > 0) {
        subarea_biomass_by_stratrum <- 
          merge(x = strata_in_subarea, 
                y = biomass_strata, 
                by = c("DESIGN_YEAR", "SURVEY", "STRATUM"))
        
        subarea_summed_biomass <- 
          stats::aggregate(cbind(BIOMASS_MT, 
                                 BIOMASS_VAR,
                                 POPULATION_COUNT, 
                                 POPULATION_VAR) ~
                             GROUP + YEAR,
                           data = subarea_biomass_by_stratrum,
                           FUN = sum)
        
        subarea_biomass <- 
          rbind(subarea_biomass,
                cbind(data.frame(SURVEY = subareas$SURVEY[isubarea],
                                 TYPE = subareas$TYPE[isubarea],
                                 AREA_NAME = subareas$AREA_ID[isubarea],
                                 DESCRIPTION = subareas$DESCRIPTION[isubarea]),
                      subarea_summed_biomass))
      }
    }
  }
  
  ## Warning Messages
  if ( "GOA" %in% subarea_biomass$SURVEY & 2023 %in% subarea_biomass$YEAR) {
      warning("The GOA total biomass across INPFC area and across depth zones
              only includes years 1987-2023. Starting from 2025, total biomass
              across NMFS areas will only be reported.")
  }
  
  ## Remove EBS + NW strata pre-1987 as these aren't used
  if (any(subarea_biomass$YEAR < 1987 & subarea_biomass$SURVEY == "EBS")) {
    warning("The (EBS + NW) output only includes years 1987-present.
      Years 1982-1986 are NOT included for the (EBS + NW) output because
      essentially no stations within strata 82 & 90 (subarea 8 & 9)
      were sampled during those years. Biomass/Abundance estimates for 
      these early years were removed.")
    
    EBS_PLUSNW_subareas <- grep(x = subarea_biomass$DESCRIPTION, 
                                pattern = "EBS Standard Plus NW Region", 
                                value = TRUE)
    
    subarea_biomass <- 
      subset(x = subarea_biomass, 
             subset = !(SURVEY == "EBS" & 
                          YEAR < 1987 & 
                          DESCRIPTION %in% EBS_PLUSNW_subareas))
  }
  
  return(subarea_biomass)
}

#' Calculate index of total biomass per stratum
#'
#' @param racebase_tables data object created from `AFSC.GAP.DBE::get_data()`
#' @param cpue object created from `AFSC.GAP.DBE::calc_cpue()`.
#' @param vulnerability the vulnerability of the species to the survey 
#'                      (defaults to 1).
#' 
#' @return dataframe of biomass and population abundance estimates across 
#'         strata, along with variances and CIs. 
#' 
#' @export
#'

calc_biomass_stratum <- function(racebase_tables = NULL,
                                 cpue = NULL,
                                 vulnerability = 1) {
  
  cruise <- racebase_tables$cruise[, c("YEAR", "SURVEY", 
                             "DESIGN_YEAR")]
  cruise <- cruise[!duplicated(cruise), ]
  
  ## Check inputs
  if (any(sapply(X = list(cpue, racebase_tables), FUN = is.null))) {
    stop("Please provide inputs for data arguments: `cpue` and `racebase_tables`. 
         See ?AFSC.GAP.DBE::calc_biomass_stratum for more details.")
  }
  
  ## Calculate mean and variance of stratum biomass. For strata with only
  ## one station, variance is ASSUMED zero
  wgt_stats <- 
    stats::aggregate(
      WGTCPUE_KG_KM2 ~ GROUP + STRATUM + YEAR + SURVEY, 
      data = cpue, 
      FUN = function(x) 
        c("MEAN_WGTCPUE_KG_KM2" = mean(x, na.rm = TRUE), 
          "VAR_WGTCPUE" = ifelse(test = length(x) < 2, 
                                 yes = 0, 
                                 no = stats::var(x) / length(x))))
  
  ## Calculate mean and variance of stratum abundance. For strata with only
  ## one station, variance is ASSUMED zero
  num_stats <- 
    stats::aggregate(
      NUMCPUE_COUNT_KM2 ~ GROUP + STRATUM + YEAR + SURVEY, 
      data = cpue,
      na.action = NULL,
      FUN = function(x) 
        c("MEAN_NUMCPUE_COUNT_KM2" = mean(x, na.rm = TRUE), 
          "VAR_NUMCPUE" = ifelse(test = length(stats::na.omit(x)) < 2, 
                                 yes = 0, 
                                 no = stats::var(x, na.rm = TRUE) / 
                                   length(stats::na.omit(x)))))
  
  ## Column merge mean wCPUE and nCPUE into one dataframe
  stratum_stats <- cbind(wgt_stats[, c("GROUP", "STRATUM", "YEAR", "SURVEY")],
                         data.frame(wgt_stats$WGTCPUE_KG_KM2),
                         data.frame(num_stats$NUMCPUE_COUNT_KM2))
  
  ## Attach DESIGN year to stratum_stats
  stratum_stats <- merge(x = stratum_stats, 
                         by.x = c("YEAR", "SURVEY"),
                         y = cruise[, c("YEAR", "SURVEY", "DESIGN_YEAR")], 
                         by.y = c("YEAR", "SURVEY"))
  
  ## Attach stratum data to stratum_stats
  stratum_stats <- merge(
    x = stratum_stats, by.x = c("SURVEY", "DESIGN_YEAR", "STRATUM"),
    y = racebase_tables$strata[, c("SURVEY", "STRATUM", "YEAR", "AREA_KM2")], 
    by.y = c("SURVEY", "YEAR", "STRATUM"))
  
  ## Calculate design-based estimate and variance of biomass and abundance
  stratum_stats[, c("BIOMASS_MT", "BIOMASS_VAR", 
                    "POPULATION_COUNT", "POPULATION_VAR")] <-
    with(stratum_stats, 
         data.frame(BIOMASS_MT = AREA_KM2 * MEAN_WGTCPUE_KG_KM2 / vulnerability * 0.001,
                    BIOMASS_VAR = AREA_KM2^2 * VAR_WGTCPUE * 1e-6,
                    POPULATION_COUNT = AREA_KM2 * MEAN_NUMCPUE_COUNT_KM2 / vulnerability,
                    POPULATION_VAR = AREA_KM2^2 * VAR_NUMCPUE))
  
  ## Reorder fields
  stratum_stats <- subset(x = stratum_stats,
                          select = c(YEAR, SURVEY, DESIGN_YEAR, STRATUM, AREA_KM2, 
                                     GROUP, 
                                     MEAN_WGTCPUE_KG_KM2, VAR_WGTCPUE, 
                                     BIOMASS_MT, BIOMASS_VAR,
                                     MEAN_NUMCPUE_COUNT_KM2, VAR_NUMCPUE,
                                     POPULATION_COUNT, POPULATION_VAR))
  
  ## Remove EBS + NW strata pre-1987 as these aren't used
  if (any(stratum_stats$YEAR < 1987 & stratum_stats$SURVEY == "EBS")) {
    warning("The (EBS + NW) output only includes years 1987-present.
      Years 1982-1986 are NOT included for the (EBS + NW) output because
      essentially no stations within strata 82 & 90 (subarea 8 & 9)
      were sampled during those years. Biomass/Abundance estimates for 
      these early years were removed.")
    
    stratum_stats <- subset(x = stratum_stats, 
                            subset = !(SURVEY == "EBS" & 
                                         YEAR < 1987 & 
                                         STRATUM %in% c(82, 90)) )
  }

  return(stratum_stats)
  
}

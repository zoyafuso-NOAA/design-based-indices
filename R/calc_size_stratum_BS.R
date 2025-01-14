#' Calculate numbers at length for each stratum
#' 
#' @description Uses Equations 16 and 17 in Wakabayashi et al. 1985 to calculate
#'              numbers at length. To be used only for the EBS_SHELF or 
#'              NBS_SHELF regions.
#' 
#' @param racebase_tables data object created from `AFSC.GAP.DBE::get_data()`.
#' @param racebase_cpue object created from `AFSC.GAP.DBE::calc_cpue()`.
#' @param racebase_stratum_popn a dataframe of stratum abundances, result 
#'                               object from  `AFSC.GAP.DBE::calc_biomass_stratum()`.
#' 
#' @export
#' 

calc_size_stratum_BS <- function(racebase_tables = NULL,
                                 racebase_cpue = NULL,
                                 racebase_stratum_popn = NULL) {
  
  ## Error Check
  if (is.null(racebase_tables$size)) 
    stop("racebase_tables$size must not be NULL. Either the taxon does not 
         have size information or rerun AFSC.GAP.DBE::get_data() with 
         argument pull_lengths = TRUE")
  
  ## Warning message if there are EBS data contained in racebase_tables
  if ("AI" %in% racebase_tables$cruise$SURVEY | 
      "GOA" %in% racebase_tables$cruise$SURVEY)
    warning("AI and/or GOA data are included in the input data. This function
       only applies the size composition to the EBS/NBS. 
       Use AFSC.GAP.DBE::calc_size_stratum_AIGOA() to calculate the size 
       composition to the AI/GOA.")
  
  ## Subset only AI/GOA data from the cruise data. If there are no AI/GOA
  ## cruises in the dataset, send out an error to use the other size comp fn. 
  cruise <- subset(x = racebase_tables$cruise,
                   subset = SURVEY %in% c("NBS", "EBS"))
  
  if (nrow(cruise) == 0){
    stop("EBS or NBS cruises are not in argument racebase_tables$cruise.
         This function only applies the size composition to the EBS/NBS. 
         Use AFSC.GAP.DBE::calc_size_stratum_AIGOA() to calculate the size 
         composition to the AI/GOA.")
  }
  
  ## Extract data objects
  
  haul <- subset(x = racebase_tables$haul,
                 subset = CRUISEJOIN %in% cruise$CRUISEJOIN)
  haul$YEAR <- as.numeric(format(haul$START_TIME, format = "%Y"))
  haul <- merge(x = haul, 
                y = cruise[, c("CRUISEJOIN", "SURVEY")])
  
  cpue <- subset(x = racebase_cpue, 
                 subset = CRUISEJOIN %in% cruise$CRUISEJOIN)
  size <- subset(x = racebase_tables$size, 
                 subset = HAULJOIN %in% haul$HAULJOIN)
  size <- merge(x = size,
                y = cpue[, c("HAULJOIN", "SURVEY")])
  
  catch <- subset(x = racebase_tables$catch,
                  subset = HAULJOIN %in% haul$HAULJOIN)
  catch$SPECIES_CODE <- catch$GROUP
  catch <- merge(x = catch,
                y = cruise[, c("CRUISEJOIN", "SURVEY")])
  
  racebase_stratum_popn <- subset(x = racebase_stratum_popn, 
                                  SURVEY %in% c("EBS", "NBS"))
  racebase_stratum_popn$SPECIES_CODE <- racebase_stratum_popn$GROUP
  
  ## Attach year and stratum information to size df
  size <- merge(x = size[, c("HAULJOIN", "SURVEY", "SPECIES_CODE", "LENGTH",
                             "FREQUENCY", "SEX")], 
                y = haul[, c("HAULJOIN", "YEAR", "STRATUM")],
                by = "HAULJOIN")
  
  ##############################################
  ## Wakabayashi et al. 1985 Equation 16: 
  ##############################################
  ## At each station with length-frequency records, the number of samples
  ##     within each sex/length class was estimated by expanding the 
  ##     length-frequency subsample to the total catch (per area swept). 
  ##     This is S_ijklm, the estimated number of individuals of sex-m and 
  ##     length-l of species-k in station-j in stratum-i.
  ##
  ## s_ijklm is the FREQUENCY column of the size df, the number of records
  ##     of sex-m and length-l of species-k recorded at station-j in stratum-i
  ## s_ijk is the frequency of recorded indidivuals in  summed over 
  ##        sex, and length class, the number of individuals of species-k 
  ##        recorded at station-j in stratum-i summed over length and sex 
  ##        classes
  ## S_ijk is the estimated numbers per area swept from the cpue df
  ##     of species-k at station-j in stratum-i. The notation S_ijk is not 
  ##     used below, instead NUMCPUE_IND_KM2 is used. 
  ##
  ## s_ijk and NUMCPUE_IND_KM2 are merged into the size df by joining 
  ##     via the HAULJOIN value and species code so that calculations can
  ##     be vectorized easily.
  
  
  s_ijk <- stats::aggregate(
    FREQUENCY ~ SURVEY + YEAR + STRATUM + HAULJOIN + SPECIES_CODE,
    data = size,
    FUN = sum)
  names(s_ijk)[names(s_ijk) == "FREQUENCY"] <- "s_ijk"
  
  size <- merge(x = size, 
                y = s_ijk[, c("SPECIES_CODE", "HAULJOIN", "s_ijk")],
                by = c("SPECIES_CODE", "HAULJOIN"))
  size <-  merge(x = size, 
                 by.x = c("HAULJOIN", "SPECIES_CODE"),
                 y = cpue[, c("HAULJOIN", "GROUP", "NUMCPUE_COUNT_KM2")],
                 by.y = c("HAULJOIN", "GROUP"))
  size$S_ijklm <- size$FREQUENCY / size$s_ijk * size$NUMCPUE_COUNT_KM2
  
  ##############################################
  ## Wakabayashi et al. 1985 Equation 17: 
  ##############################################
  ## The number of individuals (population) of species-k of sex-m and 
  ##      length-l in stratum-i is noted as P_iklm.
  ## 
  ## S_ijklm is summed over hauls-j, creating S_iklm and this is divided
  ##    by S_ik, which is S_ijklm summed over hauls-j, sex-m, and length-l.
  ## S_iklm / S_ik is the proportion of the stratum population, P_ik (noted
  ##    as racebase_stratum_popn$pop below) that is attributed to sex-m
  ##    and length-l for species-k in stratum-i. When multiplied by P_ik, 
  ##    we get P_iklm.
  ##
  ## P_ik (racebase_stratum_popn$pop) and S_ik are merged into the S_iklm
  ##    df via the year, stratum, and species_code values. Wakabayashi et al.
  ##    1985 assumes one year of data so there is no year index in their 
  ##    paper but this function has the capability of calculating size comps
  ##    for multiple years of data.
  
  S_ik <- stats::aggregate(S_ijklm ~ SURVEY + YEAR + STRATUM + SPECIES_CODE,
                           data = size,
                           FUN = sum)
  names(S_ik)[names(S_ik) == "S_ijklm"] <- "S_ik"
  
  S_iklm <- 
    stats::aggregate(S_ijklm ~ SURVEY + YEAR + STRATUM + SPECIES_CODE + 
                       LENGTH + SEX,
                     data = size,
                     FUN = sum)
  names(S_iklm)[names(S_iklm) == "S_ijklm"] <- "S_iklm" 
  
  S_iklm <- merge(x = S_iklm,
                  y = S_ik,
                  by = c("SURVEY", "YEAR", 'STRATUM', "SPECIES_CODE"))
  
  S_iklm <- merge(x = S_iklm,
                  by.x = c("SURVEY", "YEAR", 'STRATUM', "SPECIES_CODE"),
                  y = racebase_stratum_popn[c("SURVEY", "YEAR", 'STRATUM', 
                                              "GROUP", "POPULATION_COUNT")],
                  by.y = c("SURVEY", "YEAR", 'STRATUM', "GROUP"),
                  all.y = TRUE)
  
  ## For some strata in some years, lengths were not collected so the stratum
  ## population is denoted as the total population with a dummy length class
  ## of -9. A sex-class of 4 distinguishes this case, interpreted as no 
  ## individuals collected as supposed the to a sex-class of 3 which is 
  ## individuals collected but sex information not available or not sexed.
  S_iklm$LENGTH[is.na(S_iklm$LENGTH)] <- -9 ## dummy variable
  S_iklm$SEX[is.na(S_iklm$SEX)] <- 4 ## dummy variable
  S_iklm[is.na(S_iklm$S_iklm), c("S_iklm", "S_ik")] <- 1 #to ease calculation
  
  ## Population calculation
  S_iklm$NUMBER <- 
    round(x = S_iklm$POPULATION_COUNT * S_iklm$S_iklm / S_iklm$S_ik)
  
  ## Remove zero-records
  S_iklm <- subset(x = S_iklm, 
                   subset = NUMBER > 0,
                   select = -c(S_iklm, S_ik, POPULATION_COUNT))
  
  ## Widen output tables so each sex is a column
  P_iklm <- 
    stats::reshape(data = S_iklm, 
                   v.names = "NUMBER", 
                   idvar = c("SURVEY", "YEAR", 'STRATUM', "SPECIES_CODE", 
                             "LENGTH"),
                   timevar = "SEX", 
                   direction = "wide")
  
  ## Add a column of zeros if there are no unsexed individuals (NUMBER.3)
  if (!"NUMBER.3" %in% names(P_iklm)) P_iklm$NUMBER.3 <- 0
  
  names(P_iklm)[names(P_iklm) %in% paste0("NUMBER.", 1:3)] <-
    c("MALES", "FEMALES", "UNSEXED")
  
  ## Order sexes to M, F, U, TOTAL. Set NAs to zero. 
  P_iklm <- subset(P_iklm, 
                   select = c(SURVEY, YEAR, STRATUM, SPECIES_CODE, LENGTH,   
                              MALES, FEMALES, UNSEXED))
  na_idx <- is.na(P_iklm[, c("MALES", "FEMALES", "UNSEXED")])
  P_iklm[, c("MALES", "FEMALES", "UNSEXED")][na_idx] <- 0
  
  ## Calculate total over sexes
  P_iklm$TOTAL <- 
    rowSums(x = P_iklm[, c("MALES", "FEMALES", "UNSEXED")],
            na.rm = TRUE)
  
  ## Reorder rows by size
  P_iklm <- P_iklm[order(P_iklm$SURVEY, P_iklm$YEAR, P_iklm$STRATUM, 
                         P_iklm$SPECIES_CODE, P_iklm$LENGTH), ]
  
  ## Add units to LENGTH column name
  names(P_iklm)[names(P_iklm) == "LENGTH"] <- "LENGTH_MM"
  
  return(P_iklm)
}

#' Pull data from RACEBASE
#' 
#' @description Pulls cruise, haul, catch, and stratum information for the 
#'              region, years, and species of interest. 
#' 
#' @param year_set numeric or integer vector of years
#' @param survey_set character string. One of c("GOA", "AI", "EBS_SHELF", 
#'                   "EBS_SLOPE", "NBS_SHELF")
#' @param spp_codes two-column dataframe of species codes (column name 
#'                  SPECIES_CODE) and GROUP name (column name GROUP). 
#'                  For single-species, the GROUP and species codes can be the
#'                  same. Example: data.frame("SPECIES_CODE" = c(21720, 21220, 21230, 21232), 
#'                  "GROUP" = c(21720, "Grenadiers", "Grenadiers", "Grenadiers"))
#' @param haul_type integer. Defaults to haul type "3" for Standard bottom 
#'                  sample (preprogrammed station) used for biomass estimation
#' @param abundance_haul character string. "Y" are abundance hauls (what does
#'                        this mean?) and "N" are other hauls.
#' @param sql_channel connection created via AFSC.GAP.DBE::get_connected()
#' @param pull_lengths boolean T/F. Should lengths be called? Defaults to FALSE
#'                     for speed.
#' 
#' @return a named list containing cruise, haul, catch, specimen,  
#'         optional size (length), and stratum information 
#'         for the region, years, and species of interest. 
#' 
#' @export
#' 

get_data <- function(year_set = c(1996, 1999),
                     survey_set = c("GOA", "AI", "EBS", "NBS")[1],
                     spp_codes = data.frame("SPECIES_CODE" = 21720, 
                                            "GROUP" = 21720),
                     haul_type = 3,
                     abundance_haul = c("Y", "N")[1],
                     sql_channel = NULL,
                     pull_lengths = F) {
  
  ## Error Query: Check that spp_codes is a dataframe with GROUP, SPECIES_CODE
  if (class(spp_codes) != "data.frame") {
    stop("argument `spp_codes` must be a dataframe with column names 
         `GROUP` and `SPECIES_CODE`. See ?AFSC.GAP.DBE::get_data for 
         more details and examples.") 
  } else {
    if (!all(c("SPECIES_CODE", "GROUP") %in% names(spp_codes)))
      stop("argument `spp_codes` must be a dataframe with column names 
         `GROUP` and `SPECIES_CODE`. See ?AFSC.GAP.DBE::get_data for 
         more details and examples.")
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Set up channel if sql_channel = NULL
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(sql_channel)) sql_channel <- get_connected()
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Pull Cruise data: 
  ##   First . 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Error Query: argument survey_set is one the correct options.
  if (length(survey_set) == 0 |
      !all(survey_set %in% c("GOA", "AI", "EBS", "NBS"))) {
    
    survey_set[which(!survey_set %in% c("GOA", "AI", "EBS", "NBS"))]
    
    stop(paste0("arg survey_set must contain one or more of these options",
                " (case-sensitive): 
                'GOA', 'AI', 'EBS', or 'NBS'. 
                At this time, the 'EBS_SLOPE' ",
                "is not an option."))
  }
  
  cat("Pulling cruise data...\n")
  
  ## Query cruise data and filter survey regions of interest
  year_vec <- 
    paste0("(", paste0(year_set, collapse=", "), ")") 
  
  survey_def_ids <- c("AI" = 52,
                      "GOA" = 47, 
                      "EBS" = 98, 
                      "NBS" = 143)[survey_set]
  survey_def_ids_vec <- 
    paste0("(", paste0(survey_def_ids, collapse=", "), ")") 
  
  cruise_data <- 
    RODBC::sqlQuery(
      channel = sql_channel, 
      query = paste0("SELECT DISTINCT YEAR, SURVEY_DEFINITION_ID, REGION, CRUISE ",
                     "FROM RACE_DATA.V_CRUISES WHERE ",
                     "SURVEY_DEFINITION_ID IN ", survey_def_ids_vec, 
                     " AND YEAR IN ", year_vec))
  
  ## Attach CRUISEJOIN to the cruise data
  CRUISEJOIN <-     
    RODBC::sqlQuery(
      channel = sql_channel, 
      query = paste0("SELECT REGION, CRUISE, CRUISEJOIN FROM RACEBASE.CRUISE"))
  
  cruise_data <- 
    merge(x = cruise_data,
          y = CRUISEJOIN,
          by = c("REGION", "CRUISE"))
  
  ## Attach survey definition id to the cruise data
  cruise_data <-
    merge(x = cruise_data, 
          y = data.frame(SURVEY_DEFINITION_ID = survey_def_ids,
                         SURVEY = names(survey_def_ids)),
          by = "SURVEY_DEFINITION_ID")
  
  ## Attach design year to the cruise data
  cruise_data <-
    merge(x = cruise_data,
          y = AFSC.GAP.DBE::design_table,
          by = c("SURVEY", "YEAR"))
  
  ## Error Query: stop if there is no cruise data for the year and region.
  if(nrow(cruise_data) == 0) {
    stop("No data exist for survey area '", survey_set, 
         "' for the choosen set of years ", year_vec, ".")
  }
  
  
  #####################################################################
  ## Query stratum data
  #####################################################################
  cat("Pulling stratum data...\n")
  
  stratum_data <- 
    merge(x = AFSC.GAP.DBE::new_stratum_table[, c("SRVY", "YEAR", "STRATUM", 
                                                  "DESCRIPTION", "AREA_KM2")], 
          by.x = c("SRVY", "YEAR"),
          y = cruise_data[!duplicated(cruise_data[, c("SURVEY", 
                                                      "DESIGN_YEAR")]), 
                          c("SURVEY", "DESIGN_YEAR")], 
          by.y = c("SURVEY", "DESIGN_YEAR"))
  
  stratum_data <- stratum_data[order(stratum_data$SRVY, stratum_data$STRATUM),]
  names(stratum_data)[names(stratum_data) == "SRVY"] <- "SURVEY"
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Query Haul data based on the CRUISEJOIN values in the cruise_data
  ## Filter for good tows (PERFORMANCE >= 0) and haul type (e.g., 3 is the
  ##   Standard bottom sample (preprogrammed station))
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Now pulling haul data...\n")
  
  cruisejoin_vec <- 
    paste0("(", paste(cruise_data$CRUISEJOIN, collapse=", "), ")") 
  haultype_vec <- paste0("PERFORMANCE >= 0 AND HAUL_TYPE IN (", 
                         paste(haul_type, collapse=", "), ")") 
  haul_data <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM RACEBASE.HAUL ",
                                   "WHERE CRUISEJOIN IN ", cruisejoin_vec, 
                                   " AND ", haultype_vec))
  
  ## Subset years of interest based on START_TIME and abundance_haul types
  haul_data <- 
    subset(x = haul_data, 
           subset = #as.numeric(format(x = haul_data$START_TIME, 
                     #                 format = "%Y")) %in% year_set &
             haul_data$ABUNDANCE_HAUL %in% abundance_haul)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Query catch data based on cruisejoin values in haul_data and species set
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Pulling catch data...\n")
  
  avail_spp <-
    RODBC::sqlQuery(channel = sql_channel,
                    query = paste0("SELECT DISTINCT SPECIES_CODE ",
                                   "FROM RACEBASE.CATCH where CRUISEJOIN in ", 
                                   cruisejoin_vec))$SPECIES_CODE
  query_spp <- avail_spp[avail_spp %in% unique(spp_codes$SPECIES_CODE)]
  
  spp_codes_vec <- paste0("(", paste(sort(query_spp), collapse=", "), ")")
  
  catch_data <-
    RODBC::sqlQuery(channel = sql_channel,
                    query = paste0("SELECT * FROM RACEBASE.CATCH ",
                                   "where CRUISEJOIN in ", cruisejoin_vec,
                                   ifelse(test = is.null(spp_codes),
                                          yes = "",
                                          no = " and SPECIES_CODE in "),
                                   spp_codes_vec))
  
  ## Error Query: check whether there are species data
  if (!is.data.frame(catch_data))
    stop("There are no catch records for any of the species codes in argument
         spp_codes for survey area '", survey_set, "' in the chosen years ",
         year_vec)
  
  ## Merge GROUP information from spp_codes into the catch data for scenraios
  ## where you are defining a species complex.
  catch_data <- merge(x = catch_data, by.x = "SPECIES_CODE", 
                      y = spp_codes, by.y = "SPECIES_CODE")
  
  ## Aggregate weights and numbers of fish by GROUP and HAULJOIN. 
  catch_data <- stats::aggregate( 
    cbind(WEIGHT, NUMBER_FISH) ~ 
      HAULJOIN + REGION + CRUISE + GROUP,
    data = catch_data,
    na.rm = TRUE, na.action = NULL,
    FUN = sum)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Query Size information if pull_lengths == TRUE
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  size_data = NULL
  if (pull_lengths) {
    cat("Pulling size data...\n")
    size_data <- 
      RODBC::sqlQuery(channel = sql_channel, 
                      query = paste0("SELECT * FROM RACEBASE.LENGTH ",
                                     "where CRUISEJOIN in ", cruisejoin_vec,
                                     ifelse(test = is.null(spp_codes),
                                            yes = "",
                                            no = " and SPECIES_CODE in "),
                                     spp_codes_vec)) 
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Query Specimen information
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  speclist <- RODBC::sqlQuery(
    channel = sql_channel, 
    query = paste0("select s.SPECIES_CODE, s.cruisejoin, s.hauljoin, ",
                   "s.region, s.vessel, s.cruise, s.haul, s.specimenid, ",
                   "s.length, s.sex, s.weight, s.age  from ",
                   "racebase.specimen s where ",
                   # "REGION in ", region_vec, " and ",
                   "CRUISEJOIN in ", cruisejoin_vec, " and ",
                   "SPECIES_CODE in ", spp_codes_vec))
  
  ## Error Query: send out a warning if there are no ages in the dataset
  if (length(table(speclist$AGE)) == 0)
    warning("There are no age data for the species_codes in spp_codes for 
            survey area '", survey_set, "' in the chosen years ", year_vec)
  
  #####################################################################
  ## Query species information
  #####################################################################
  cat("Pulling species data...\n")
  species_info <- 
    RODBC::sqlQuery(channel = sql_channel, 
                    query = paste0("SELECT * FROM RACEBASE.SPECIES where ",
                                   ifelse(test = is.null(spp_codes),
                                          yes = "",
                                          no = " SPECIES_CODE in "),
                                   spp_codes_vec))
  
  ## Merge GROUP information
  species_info <- merge(x = species_info, 
                        y = spp_codes,
                        by.x = "SPECIES_CODE", by.y = "SPECIES_CODE")
  
  cat("Finished.\n")
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Put cruise, haul, catch, stratu, and species data into a list and return
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(list(cruise = cruise_data,
              haul = haul_data,
              catch = catch_data,
              size = size_data,
              strata = stratum_data,
              species = species_info,
              specimen = speclist))
  
}

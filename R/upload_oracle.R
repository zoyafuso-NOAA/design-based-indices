#' Upload a table to Oracle with associated metadata
#' 
#' @description T
#'
#' @param x Either a character string of the path to the .csv file or a data.frame
#' @param table_name Name of the table. (Add error checks to make sure table name follows any peculiarities of Oracle tables)
#' @param table_metadata Description of what the table is
#' @param metadata_column data.frame describing the metadata for each of the 
#'                        fields in the table. Must contain these columns: 
#'                        1) colname: name of field
#'                        2) colname_long: longer version of name for printing
#'                           purposes.
#'                        3) units: units of field
#'                        4) dataype: Oracle data type
#'                        5) colname_desc: Full description of field
#' @param channel Establish your oracle connection using a function 
#'                like AFSC.GAP.DBE::get_connected() 
#' @param schema character string. The name of the schema to save table. 
#'               "GAP_PRODUCTS" is the schema where production tables will live.   
#' @param update_table boolean. Default = TRUE. Save or drop and save the 
#'                     table in Oracle. 
#' @param update_metadata boolean. Default = TRUE. Add table and column 
#'                        metadata to the tables. 
#' @param share_with_all_users boolean. Default = TRUE. Give all users in 
#'                             oracle view permissions. 
#'
#' @return
#' @export
#' 

upload_oracle <- function(x = NULL,
                          table_name = NULL, 
                          metadata_column = NULL, 
                          table_metadata = NULL,
                          channel = NULL, 
                          schema = NULL, 
                          update_table = TRUE, 
                          update_metadata = TRUE,
                          share_with_all_users = TRUE) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Initial Error Checks
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Error Checks for NULLs
  for (ivar in c("x", "table_name", "metadata_column", "table_metadata")) 
    if (is.null(x = get(x = ivar))) 
      stop(paste0("Must provide argument `", ivar, "`."))
  
  ## Check that x is a data.frame. If a path is provided, read the path.
  if (is.character(x = x)) x <- utils::read.csv(file = x)
  if (!is.data.frame(x = x)) stop("Please supply a data.frame for argument `x`")
  
  ## Check that metadata_column is a dataframe with columns colname, 
  ## colname_long, units, datatype, and colname_desc
  if (!is.data.frame(x = metadata_column)){
    stop(
      "Argument `metadata_column` must be a data.frame.
       See AFSC.GAP.DBE::oracle_upload() for how to format `metadata_column`.")
  } else {
    if (!all(c("colname", "colname_long", "units", "datatype", "colname_desc")
             %in% names(metadata_column))) 
      stop(
        "Argument `metadata_column` must be a data.frame with fields:
        1) colname: name of field
        2) colname_long: longer version of name for printing purposes.
        3) units: units of field
        4) dataype: Oracle data type
        5) colname_desc: Full description of field")
  }
  
  ## Check that there is a connection
  if (is.null(channel)) channel <- AFSC.GAP.DBE::get_connected()
  
  cat(paste0("Oracle Data Table: ", schema, ".", table_name, 
             "\nNumber of Rows: ", nrow(x = x), 
             "\nNumber of Fields with Metadata: ", 
             sum(!is.na(x = metadata_column$colname)), "\n\n"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Update Table
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (update_table) {
    start_time <- Sys.time()
    cat(paste0("Updating Table ", table_name, " ... "))
    ## If table is currently in the schema, drop the table before re-uploading
    existing_tables <- 
      unlist(RODBC::sqlQuery(channel = channel, 
                             query = "SELECT table_name FROM user_tables;")) 
    if (table_name %in% existing_tables) 
      RODBC::sqlDrop(channel = channel, sqtable = table_name)
    
    ## Format metadata as a named vector to be inputted as argument 
    ## `varTypes` in RODBC::sqlSave()
    metadata_column <- subset(x = metadata_column,
                              subset = !is.na(colname))
    
    vartype_vec <- stats::setNames(object = metadata_column$datatype,
                                   nm = metadata_column$colname)
    
    ## Assign the dataframe `x` to the table_name
    assign(x = table_name, value = x)
    
    ## Add the table to the schema
    eval(parse(text = paste0("RODBC::sqlSave(channel = channel, dat = ",
                             table_name, ", varTypes = vartype_vec, ",
                             "rownames = FALSE)")))
    
    end_time <- Sys.time()
    cat(paste("Time Elapsed:", round(end_time - start_time, 2), 
              units(end_time - start_time), "\n"))
  }

  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Update Metadata
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (update_metadata) {
    cat("Updating Metadata ...\n")
    ## Add column metadata 
    if (nrow(x = metadata_column) > 0) {
      for (i in 1:nrow(x = metadata_column)) {
        
        desc <- gsub(pattern = "<sup>2</sup>",
                     replacement = "2",
                     x = metadata_column$colname_long[i], 
                     fixed = TRUE)
        short_colname <- gsub(pattern = "<sup>2</sup>", 
                              replacement = "2",
                              x = metadata_column$colname[i], 
                              fixed = TRUE)
        
        RODBC::sqlQuery(
          channel = channel,
          query = paste0('comment on column ', 
                         schema, '.', table_name,'.',
                         short_colname,' is \'',
                         desc, ". ", # remove markdown/html code
                         gsub(pattern = "'", replacement ='\"',
                              x = metadata_column$colname_desc[i]),'\';'))
        
      }
    }
    ## Add table metadata 
    RODBC::sqlQuery(
      channel = channel,
      query = paste0('comment on table ', schema,'.', table_name,
                     ' is \'',
                     table_metadata,'\';'))
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Grant select access to all users
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (share_with_all_users) {
    start_time <- Sys.time()
    cat("Granting select access to all users ... ")
    all_schemas <- RODBC::sqlQuery(channel = channel,
                                   query = paste0('SELECT * FROM all_users;'))
    
    for (iname in sort(all_schemas$USERNAME)) {
      RODBC::sqlQuery(channel = channel,
                      query = paste0('grant select on ', schema,'.', table_name,
                                     ' to ', iname, ';'))
    }
    
    end_time <- Sys.time()
    cat(paste("Time Elapsed:", round(end_time - start_time, 2), 
              units(end_time - start_time), "\n"))
  }
cat("Finished\n")
}

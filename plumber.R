#* @apiTitle SunnyD Flooding API
#* @apiDescription This API contains functions for the SunnyD Flooding project, which measures and models urban coastal flooding
#* @apiContact list(name = "API Support", email = "gold@unc.edu")
#* @apiVersion 1.0.1

#----------------- Expose mounted storage "/data" as the "public folder" that will store images sent to API
#* @assets /data
list()

#------------------ Read water level ----------------
#* Read water level for sites
#* @param key API keys
#* @param min_date Minimum date (yyyymmdd)
#* @param max_date Maximum date (yyyymmdd)
#* @param sensor_id Sensor ID (typically: site acronym, "_", two-digit number)
#* @get /read_water_level
function(key = "", min_date="", max_date="", sensor_id=""){
  min_date_parsed = lubridate::ymd(min_date)
  max_date_parsed = lubridate::ymd(max_date)

  if(min_date_parsed > max_date_parsed){
    stop("min_date LARGER THAN max_date")
  }

  if(is.na(min_date_parsed) | is.na(max_date_parsed)){
    stop("min_date OR max_date in wrong format. must be 'yyyymmddd'!")
  }

  if(!sensor_id %in% sensor_id_list){
    stop("sensor_id IS INVALID")
  }

  if(!key %in% api_keys){
    stop("WRONG KEY!")
  }

  if(key %in% api_keys){
    con %>%
      tbl("sensor_data") %>%
      filter(sensor_ID %in% sensor_id,
             date >= min_date_parsed & date <= max_date_parsed) %>%
      collect()
  }
}

#------------------ Read last water level ----------------
#* Read last water level for sites
#* @param key API keys
#* @param sensor_id Sensor ID (typically: site acronym, "_", two-digit number)
#* @get /latest_water_level
function(key = "", sensor_id=""){
  if(!sensor_id %in% sensor_id_list){
    stop("sensor_id IS INVALID")
  }

  if(!key %in% api_keys){
    stop("WRONG KEY!")
  }

  if(key %in% api_keys){
    con %>%
      tbl("sensor_data") %>%
      filter(sensor_ID %in% sensor_id,
             date == max(date, na.rm=T)) %>%
      collect()
  }
}


#-------------- Write water level -----------------
#* Write water level for sites
#* @param key API Key
#* @param place City name, State name of location
#* @param sensor_id Sensor ID
#* @param dttm Datetime of sample (yyyymmddhhmmss)
#* @param level Water level relative to ground
#* @param voltage Voltage of sensor
#* @param notes Misc notes about sample
#* @post /write_water_level
function(key = "", place="", sensor_id="", dttm="", level="", voltage="", notes=""){
  date_parsed = lubridate::ymd_hms(dttm, tz = "EST")

  if(is.na(date_parsed)){
    stop("min_date OR max_date in wrong format. must be 'yyyymmddhhmmss'!")
  }

  if(!sensor_id %in% sensor_id_list){
    stop("sensor_id IS INVALID")
  }

  if(!key %in% api_keys){
    stop("WRONG KEY!")
  }

  if(key %in% api_keys){

    dbAppendTable(conn = con,
                  name = "sensor_data",
                  value = tibble(
                    "place" = place,
                    "sensor_ID" = sensor_id,
                    "date" = date_parsed,
                    "level" = level,
                    "voltage" = voltage,
                    "notes" = notes
                  )
    )
    return("SUCCESS!")
  }
}


#--------------- Upload tab separated file of values --------------
#* @post /upload_sensor_data_tsv
#* @serializer text
#* @parser multi
#* @parser text
#* @param key API key
#* @param file:file A file
#* Upload a tab-separated text file and save to database "sensor_data"
function(key, file) {

  if (key %in% api_keys) {
    sanitizedFile <- gsub("\\W", "", file)

    new_file <- tempfile()
    writeLines(file[[1]], new_file)

    df <- read_tsv(new_file)
    unlink(new_file)

    if(!identical(colnames(df), colnames(con %>% tbl("sensor_data")))){
      stop("ERROR: COLUMNS NAMES ARE NOT EQUAL. CHECK FILE STRUCTURE. IS IT A TAB-SEPARATED TEXT FILE?")
    }

    if(identical(colnames(df), colnames(con %>% tbl("sensor_data")))){

      max_date_df <- con %>%
        tbl("sensor_data") %>%
        filter(sensor_ID %in% !!df$sensor_ID) %>%
        # collect() %>%
        group_by(sensor_ID) %>%
        summarise(max_date = max(date, na.rm=T)) %>%
        collect()

      sensor_ID_list <- unique(df$sensor_ID)

      filtered_df <- foreach(i=1:length(sensor_ID_list), .combine = "bind_rows") %do% {
        df %>%
          filter(sensor_ID == sensor_ID_list[i]) %>%
          filter(date > max_date_df$max_date[max_date_df$sensor_ID == sensor_ID_list[i]])
      }

      if(is.null(filtered_df) | nrow(filtered_df) == 0){
        stop("THE SUPPLIED .TXT FILE CONTAINS INFO ALREADY IN DATABSE")
      }

      if(!is.null(filtered_df) & nrow(filtered_df > 0)){
        dbAppendTable(conn = con,
                      name = "sensor_data",
                      value = filtered_df
        )
        return("Success: wrote to table!")
      }
    }
  }
}

#------------------------- Upload .jpegs or .jpgs -----------------------
#* @post /upload_picture
#* @parser multi
#* @parser jpeg
#* @param key API key
#* @param file:file A file
#* Upload a jpeg
function(key, camera_ID, file) {

  if (key %in% api_keys & camera_ID %in% camera_id_list) {

    new_file <- tempfile(fileext = ".jpg")
    magick::image_write(file[[1]], new_file)

    magick::image_write(file[[1]], paste0("/data/",camera_ID,".jpg"))
    
    gc()

    # Future code for onedrive API here.

  }
}

#------------------------- Get info of latest picture for a site -----------------------
#* @get /get_latest_picture_info
#* @param key API key
#* Get latest picture info
function(key, camera_ID) {

  if (key %in% api_keys & camera_ID %in% camera_id_list) {

      return(file.info(paste0("/data/",camera_ID,".jpg"))$mtime)
    }
}



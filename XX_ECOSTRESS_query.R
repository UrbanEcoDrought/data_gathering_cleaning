# Script to query ECOSTRESS data from the APPEEARS API
# APPEEARS: https://appeears.earthdatacloud.nasa.gov/
# ECOSTRESS: https://ecostress.jpl.nasa.gov/

# Libraries----
library(httr)
library(jsonlite)
library(readxl)

# using template script from pgasana@anl.gov from the Census API query.
# tryCatch({
#   # COUNTY LEVEL PER CAPITA INCOME
#   asc_json <- fromJSON(
#     txt = "https://api.census.gov/data/2019/acs/acs5?get=group(B19301)&for=county"
#   )
#   
#   asc_percapita_df <- setNames(
#     data.frame(asc_json[2:nrow(asc_json),]), 
#     as.vector(asc_json[1,])
#   )
#   
#   asc_percapita_df <- within(
#     asc_percapita_df[c("GEO_ID", "NAME", "state", "county", "B19301_001E", "B19301_001M")], 
#     {
#       B19301_001E <- as.integer(B19301_001E)
#       B19301_001M <- as.integer(B19301_001M)
#       acs_pci_usa <- as.numeric(asc_national_percapita_vec["B19301_001E"])
#       type <- as.factor("acs")
#       threshold_80_pct_nat_indicator <- ifelse(
#         B19301_001E >= 0.8*as.numeric(asc_national_percapita_vec["B19301_001E"]), 
#         "above",
#         "below"
#       )
#       threshold_80_pct_nat_value <- 0.8*as.numeric(asc_national_percapita_vec["B19301_001E"])
#       GeoFips <- paste0(state, county)
#     }
#   )
#   
#   rm(asc_json, asc_national_percapita_vec)
# }, warning = function(w) w
# , error = function(e) e
# )

####################################
# ECOSTRESS Data----

# Login to earth data cloud----
# putting in my credentials to make a task query
secret <- base64_enc(paste("alexanderm10", "N@thingi$True01", sep=":"))

# establishing a token to make data queries
# Token will expire after 48hrs, so we should probably just run this code each time if it doesn't break things
response <- POST("https://appeears.earthdatacloud.nasa.gov/api/login",
                 add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                             "cContent-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                 body = "grant-type=client_credentials")

token_response <- prettify(toJSON(content(response), auto_unbox = T))
token_response

# Ecostress Repository list----
# looking into the ECOSTRESS repository adn seeign what's there
ecostress_json <- fromJSON(
    txt = "https://appeears.earthdatacloud.nasa.gov/api/product"
  )

  # Subsetting to list only datasets that are from ECOSTRESS
  ecostress.dat <- ecostress_json[ecostress_json$Platform=="ECOSTRESS",]
  names(ecostress.dat)
  ecostress.dat
  # Looking into a given product
  ecostress_json2 <- fromJSON(
    txt = "https://appeears.earthdatacloud.nasa.gov/api/product/ECO2LSTE.001"
  )  
  names(ecostress_json2) # can see all fo the different variables we can query here
  

# Submitting a Task Request----
  # I think this is what we will have to do to query the given area that we want to look into around chicago
  # web resource: https://appeears.earthdatacloud.nasa.gov/api/?r#submit-task

  # could write a text file with the task if it is going to be a recurring thing.
  
  # load task request from file
  task <- toJSON(read_json("task_request.json"), auto_unbox = T)
  
  # submit task
  token <- paste("Bearer", fromJSON(token_response)$token)
  response <- POST("https://appeears.earthdatacloud.nasa.gov/api/task", body = task, encode = "json", 
                   add_headers(Authorization = token, "Content-Type" = "application/json"))
  task_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
  task_response

  task.list<- fromJSON(task_response)
  task.id <- task.list$task_id
  
  # # Checking  if any tasks are processing
  token <- paste("Bearer", fromJSON(token_response)$token)
  response <- GET("https://appeears.earthdatacloud.nasa.gov/api/status", add_headers(Authorization = token))
  status_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
  status_response
  

## Bundle----
  # the task generates a bundle of data that we will now have to download to access
  
  # This will get us a list of the files that are contained within the bundle
  # need to figure out how to get the task_id from the task_response output automatically.
  token <- paste("Bearer", fromJSON(token_response)$token)
  task_id <- task.id
  response <- GET(paste("https://appeears.earthdatacloud.nasa.gov/api/bundle/", task_id, sep = ""), add_headers(Authorization = token))
  bundle_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
  bundle_response
  
  # storing bundle_response
  bundle.list <- fromJSON(bundle_response)
  
  # gettign list of file names  
  bundle.list$files
  file.id <- c(bundle.list$files$file_id)
  
  for(i in unique(file.id)){
    task_id <- task.id
    file_id <- i
    
    token <- paste("Bearer", fromJSON(token_response)$token)
    
    filename <- bundle.list$files[bundle.list$files$file_id==i, "file_name"]
    
    filepath <- file.path("input_data", "test",filename) 
    suppressWarnings(dir.create(dirname(filepath)))
    
    response <- GET(paste("https://appeears.earthdatacloud.nasa.gov/api/bundle/", task_id, '/', file_id, sep = ""),
                    write_disk(filepath, overwrite = TRUE), progress(), add_headers(Authorization = token))
  }
  
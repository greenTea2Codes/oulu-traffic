library(jsonlite)
library(RPostgreSQL)

# define variables for handling database exception
dbError <- FALSE
dbErrorMsg <- character()

# connect to a local postgreDB
pw <- { 
  "myPostgres" 
}
postgreDriver <- dbDriver("PostgreSQL")
connection <- tryCatch(
  # test database exception
  # dbConnect(postgreDriver, dbname = "dalun",host = "localhost", port = 5431, user = "dalun", password = pw),
  
  dbConnect(postgreDriver, dbname = "dalun",host = "localhost", port = 5432, user = "dalun", password = pw),
  error = function(e){
    print(e)
    dbError <<- TRUE
    dbErrorMsg <<- e
  }
)

# remove the password from the environment
rm(pw)

# create an empty dataframe mapping the parking_station table
df_parkingstations <- data.frame(
  station_id <- integer(length = 0),
  geom <- character(),
  name <- character()
)
# fetch data from the table parking_station
if(dbError != TRUE){
  df_parkingstations <- dbGetQuery(connection, 
                                   "SELECT * from parking_station")
}

# create a dataframe for storing details for each station
# and mapping the table freespaces_update
df_parkingstations_details <- data.frame()
df_parkingstations_details$name <- character()
df_parkingstations_details$timestamp <- character()
df_parkingstations_details$address <- character()
df_parkingstations_details$freespaces <- integer()
df_parkingstations_details$totalspaces <- integer()

# variables for api error handling
apiError <- FALSE
apiErrorMsg <- character()

# fetch parking station details from an API
if(nrow(df_parkingstations) > 0){
  for(i in 1:nrow(df_parkingstations)){
    api_url <- paste0("https://www.oulunliikenne.fi/public_traffic_api/parking/parking_details.php?parkingid=", as.character(df_parkingstations[i, 1]), collapse = NULL)
    # for testing
    # if(i == 2){
    #   api_url <- paste0("https://www.oulunliikenne.fi/public_traffic_api/parking_details.php?parkingid=", as.character(df_parkingstations[i, 1]), collapse = NULL)
    # }
    parkingstation_details <- tryCatch(
      fromJSON(api_url), 
      error = function(e){
        apiError <<- TRUE
        apiErrorMsg <<- paste(api_url,"<br>",
                              e, "<br>",
                              "<br>",
                              "Fetched data is incomplete,","<br>",
                              "therefore it has not been stored in the database")
        # apiErrorMsg <<- e
      });
    # print(paste("apiError at line 59: ", apiError))
    if(apiError == TRUE) break;
    df_parkingstations_details[i, 1] <- parkingstation_details$name
    df_parkingstations_details[i, 2] <- parkingstation_details$timestamp
    df_parkingstations_details[i, 3] <- parkingstation_details$address
    if("freespace" %in% names(parkingstation_details)){
      df_parkingstations_details[i, 4] <- as.integer(parkingstation_details$freespace)
      df_parkingstations_details[i, 5] <- as.integer(parkingstation_details$totalspace)
    } else {
      df_parkingstations_details[i, 4] <- 0
      df_parkingstations_details[i, 5] <- 0
    }
  }
}
# write data to the table 
if(dbError != TRUE && apiError != TRUE){
  dbWriteTable(connection, "freespaces_update", 
               value = df_parkingstations_details, 
               append = TRUE, row.names = FALSE)
  print("task complete")
}

# close db connection
if(dbError != TRUE){
  dbDisconnect(connection)
  dbUnloadDriver(postgreDriver)
}
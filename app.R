library(shiny)
library(jsonlite)
library(RPostgreSQL)
# library(curl)

# connect to a local postgreDB
pw <- { 
  "myPostgres" 
}
postgreDriver <- dbDriver("PostgreSQL")
connection <- dbConnect(postgreDriver, dbname = "dalun",
                        host = "localhost", port = 5432,
                        user = "dalun", password = pw)

# remove the password from the environment
rm(pw)

# fetch data from the table parking_station
df_parkingstations <- dbGetQuery(connection, 
                          "SELECT * from parking_station")

# create a dataframe for storing details for each station
# and mapping the table freespaces_update
df_parkingstations_details <- data.frame()
df_parkingstations_details$name <- character()
df_parkingstations_details$timestamp <- character()
df_parkingstations_details$address <- character()
# df_parkingstations_details$timestamp <- timestamp()
df_parkingstations_details$freespaces <- integer()
df_parkingstations_details$totalspaces <- integer()

# fetch parking station details from an API
for(i in 1:nrow(df_parkingstations)){
  api_url <- paste0(paste0("https://www.oulunliikenne.fi/public_traffic_api/parking/parking_details.php?parkingid=", as.character(df_parkingstations[i, 1]), collapse = NULL))
  parkingstation_details <- fromJSON(api_url)
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

# fetch the last row in the freespaces_update table
last_row_freespaces_update <- dbGetQuery(connection, "SELECT timestamp FROM freespaces_update WHERE name = 'Ouluhalli' ORDER BY timestamp DESC LIMIT 1")

# write data to the table 
dbWriteTable(connection, "freespaces_update", 
             value = df_parkingstations_details, 
             append = TRUE, row.names = FALSE)

# close db connection
dbDisconnect(connection)
dbUnloadDriver(postgreDriver)

parkingPlaces <- fromJSON("https://www.oulunliikenne.fi/public_traffic_api/parking/parkingstations.php")
parkingPlaces$parkingstation$address <- "address"
parkingPlaces$parkingstation$timestamp <- "timestamp"
parkingPlaces$parkingstation$freespace <- "0"
parkingPlaces$parkingstation$totalspace <- "0"
#timeFetched <- "time"
#totalParkingPlaces <- nrow(parkingPlaces$parkingstation)
for(i in 1:length(parkingPlaces$parkingstation$id)){
  detailUrl <- paste0("https://www.oulunliikenne.fi/public_traffic_api/parking/parking_details.php?parkingid=", as.character(parkingPlaces$parkingstation[i, 1]), collapse = NULL)
  parkingPlace <- fromJSON(detailUrl)
  # if(i == 1){
  #   timeFetched <<- parkingPlace$timestamp
  # }
  if("freespace" %in% names(parkingPlace)){
    parkingPlaces$parkingstation[i,4] <- parkingPlace$address
    parkingPlaces$parkingstation[i,5] <- parkingPlace$timestamp
    parkingPlaces$parkingstation[i,6] <- parkingPlace$freespace
    parkingPlaces$parkingstation[i,7] <- parkingPlace$totalspace
  } else {
    parkingPlaces$parkingstation[i,4] <- parkingPlace$address
    parkingPlaces$parkingstation[i,5] <- parkingPlace$timestamp
    #next
    #parkingPlaces$parkingstation[i,4] <- "0"
  }
}

# # Function to fetch free spaces
# fetchFreeSpaces <- function(){
#   for(i in 1:length(parkingPlaces$parkingstation$id)){
#     detailUrl <- paste0("https://www.oulunliikenne.fi/public_traffic_api/parking/parking_details.php?parkingid=", as.character(parkingPlaces$parkingstation[i, 1]), collapse = NULL)
#     parkingPlace <- fromJSON(detailUrl)
#     # if(i == 1){
#     #   timeFetched <<- parkingPlace$timestamp
#     # }
#     if("freespace" %in% names(parkingPlace)){
#       #parkingPlaces$parkingstation[i,4] <- parkingPlace$address
#       parkingPlaces$parkingstation[i,5] <- parkingPlace$timestamp
#       parkingPlaces$parkingstation[i,6] <<- parkingPlace$freespace
#       #parkingPlaces$parkingstation[i,6] <- parkingPlace$totalspace
#     } else {
#       #parkingPlaces$parkingstation[i,4] <- parkingPlace$address
#       next
#       #parkingPlaces$parkingstation[i,4] <- "0"
#     }
#   }
# }

# UI
ui <- fluidPage(
  # CSS
  tags$head(
    tags$style(HTML("
      #result {
        color: blue;
      }
    ")
    )
  ),
  
  # Application title
  titlePanel("Free parking spaces in Oulu"),
  fluidRow(
    column(
      3,
      #plotOutput("barChart")
      h2("Select a parking place to see the details:"),
      selectInput("selectedParkingPlace", NULL,
                  choices = parkingPlaces$parkingstation$name,
                  selected = NULL),
      htmlOutput("result")
      #textOutput("result")
    ),
    column(
      9,
      plotOutput("barChart"),
      h2("Combined data from a local postgreDB and an Oulu city API"),
      # h2("Original Data from Oulu city:"),
      # textOutput("timeFetched"),
      tableOutput("table"),
      p("station_id, geom and name are from the local DB, whereas timestamp, address, freespaces and total spaces are from the API below:"),
      # p("Data is fetched from the APIs below:"),
      # tags$a(href="https://www.oulunliikenne.fi/public_traffic_api/parking/parkingstations.php", "www.oulunliikenne.fi/public_traffic_api/parking/parkingstations.php"),
      # tags$br(),
      tags$a(href="https://www.oulunliikenne.fi/public_traffic_api/parking/parking_details.php?parkingid=2", "www.oulunliikenne.fi/public_traffic_api/parking/parking_details.php?parkingid=2")
    )
  )
)

# Define required server logic
server <- function(input, output) {
  
  output$barChart <- renderPlot({
    freespaces <- as.numeric(parkingPlaces$parkingstation$freespace)
    barplot(freespaces, xlab ="parking places", ylab="free spaces", names.arg = parkingPlaces$parkingstation$name, col = "orange")
  })
  # output$timeFetched <- renderText(timeFetched)
  output$table <- renderTable(df_parkingstations_details)
  # output$table <- renderTable(df_parkingstations)
  # output$table <- renderTable(parkingPlaces$parkingstation)
  output$result <- renderText({
    parkingPlaceDetails <- parkingPlaces$parkingstation[parkingPlaces$parkingstation$name == input$selectedParkingPlace,]

    paste("On", parkingPlaceDetails$timestamp,"<br>", parkingPlaceDetails$name, "has" , parkingPlaceDetails$freespace, "free spaces")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


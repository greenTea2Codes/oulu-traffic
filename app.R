library(shiny)
library(jsonlite)
library(RPostgreSQL)

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
df_parkingstations_details$freespaces <- integer()
df_parkingstations_details$totalspaces <- integer()

# fetch parking station details from an API
for(i in 1:nrow(df_parkingstations)){
  api_url <- paste0("https://www.oulunliikenne.fi/public_traffic_api/parking/parking_details.php?parkingid=", as.character(df_parkingstations[i, 1]), collapse = NULL)
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

# write data to the table 
dbWriteTable(connection, "freespaces_update", 
             value = df_parkingstations_details, 
             append = TRUE, row.names = FALSE)

# close db connection
dbDisconnect(connection)
dbUnloadDriver(postgreDriver)

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
      h2("Select a parking place to see the details:"),
      selectInput("selectedParkingPlace", NULL,
                  choices = df_parkingstations$name,
                  selected = NULL),
      htmlOutput("result")
    ),
    column(
      9,
      plotOutput("barChart"),
      h2("Data fetched from Oulu city API:"),
      tableOutput("table"),
      p("Above data is from the API below (the id of the station is used in query parameter):"),
      tags$a(href="https://www.oulunliikenne.fi/public_traffic_api/parking/parking_details.php?parkingid=2", "www.oulunliikenne.fi/public_traffic_api/parking/parking_details.php?parkingid=2")
    )
  )
)

# Define required server logic
server <- function(input, output) {
  output$barChart <- renderPlot({
    freespaces <- as.numeric(df_parkingstations_details$freespaces)
    barplot(freespaces, xlab ="parking places", ylab="free spaces", names.arg = df_parkingstations_details$name, col = "orange")
  })
  output$table <- renderTable(df_parkingstations_details, digits = 0)
  output$result <- renderText({
    parkingPlaceDetails <- df_parkingstations_details[df_parkingstations_details$name == input$selectedParkingPlace,]
    paste("On", parkingPlaceDetails$timestamp,"<br>", parkingPlaceDetails$name, "has" , parkingPlaceDetails$freespaces, "free spaces")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(jsonlite)
library(curl)

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
      h2("Original Data from Oulu city:"),
      # textOutput("timeFetched"),
      tableOutput("table"),
      p("Data is fetched from the APIs below:"),
      tags$a(href="https://www.oulunliikenne.fi/public_traffic_api/parking/parkingstations.php", "www.oulunliikenne.fi/public_traffic_api/parking/parkingstations.php"),
      tags$br(),
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
  output$table <- renderTable(parkingPlaces$parkingstation)
  output$result <- renderText({
    parkingPlaceDetails <- parkingPlaces$parkingstation[parkingPlaces$parkingstation$name == input$selectedParkingPlace,]

    paste("On", parkingPlaceDetails$timestamp,"<br>", parkingPlaceDetails$name, "has" , parkingPlaceDetails$freespace, "free spaces")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


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

# Function to fetch free spaces
fetchFreeSpaces <- function(){
  for(i in 1:length(parkingPlaces$parkingstation$id)){
    detailUrl <- paste0("https://www.oulunliikenne.fi/public_traffic_api/parking/parking_details.php?parkingid=", as.character(parkingPlaces$parkingstation[i, 1]), collapse = NULL)
    parkingPlace <- fromJSON(detailUrl)
    # if(i == 1){
    #   timeFetched <<- parkingPlace$timestamp
    # }
    if("freespace" %in% names(parkingPlace)){
      #parkingPlaces$parkingstation[i,4] <- parkingPlace$address
      parkingPlaces$parkingstation[i,5] <- parkingPlace$timestamp
      parkingPlaces$parkingstation[i,6] <<- parkingPlace$freespace
      #parkingPlaces$parkingstation[i,6] <- parkingPlace$totalspace
    } else {
      #parkingPlaces$parkingstation[i,4] <- parkingPlace$address
      next
      #parkingPlaces$parkingstation[i,4] <- "0"
    }
  }
}

# UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Free parking spaces in Oulu:"),
  fluidRow(
    column(
      12,
      plotOutput("barChart")
    ),
    column(
      12,
      h2("Original Data from Oulu city:"),
      # textOutput("timeFetched"),
      tableOutput("table")
    )
  )
)

# Define required server logic
server <- function(input, output) {
  
  output$barChart <- renderPlot({
    freespaces <- as.numeric(parkingPlaces$parkingstation$freespace)
    barplot(freespaces, xlab ="parking places", ylab="free spaces", names.arg = parkingPlaces$parkingstation$name)
  })
  # output$timeFetched <- renderText(timeFetched)
  output$table <- renderTable(parkingPlaces$parkingstation)
}

# Run the application 
shinyApp(ui = ui, server = server)


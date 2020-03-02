#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rjson)
library(httr)

single_adr = function(adr){
  if(adr=="" | grepl("[][!#$%()*.:;<=>@^_`|~.{}]`", adr)==TRUE){
    print("Please check again your input")
  }else{
    url<- "http://www.datasciencetoolkit.org/maps/api/geocode/json"
    json <-fromJSON(content(GET(url,query=list(sensor="FALSE",address=adr)),type="text"))
    if (json$status=="OK"){
      loc  <- json$results[[1]]$geometry$location
      return(c(address=adr,long=loc$lng, lat= loc$lat))
    }else {
      return("Status Failed Please check again your adress")
    }
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Sidebar title for shiny app
  titlePanel("GeoCode Google Cordinates"),
  
  
  # Sidebar panel for inputs
  sidebarPanel(
    
    # Longitude search, default is LiU can be changed in shiny app
    textInput("txtoutp", "Address:"),
    
    # # Latitude search, default is LiU, can be changed in shiny app
    # numericInput("lat", "latitude:", 58.398, min = -90, max = 90),
    
    # Adds an action button to trigger code chunks. Limits no of calls to API(!)
    actionButton("act", "Search!")
  )
  
  
)

# Main panel for displaying outputs
mainPanel(
  #   df <- data.frame(
  #     address = c("Antwerp","Haarlem","Dordrecht"),
  #     stringsAsFactors = FALSE
  #   ),
  #   mutate_geocode(df, address),
  # 
  #   df %>% mutate_geocode(address),
  # 
  #   locations <- as_tibble(df),
  #   locations <- as_tibble(df)
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$act, {
  
    aaa <- single_adr("input$txtoutp")
    cat("address is:", input$aaa, "rows\n")
    
 
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




leaflet() %>% setView(lng=42, lat=23, zoom=8 ) %>%addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
  addCircleMarkers(data=m, lng=~Longtitude , lat=~Latitude, radius=8 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Red")


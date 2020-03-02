library(shiny)
library(tidyverse)

df<-read.table("SENIC.txt")
dt<-df[c(-1,-8,-9)]
plot=list()


ui <- fluidPage(

  titlePanel("Density and Histogram Plot"),

  sidebarLayout(

    sidebarPanel(
      sliderInput(inputId = "bins",
                  label="Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      checkboxGroupInput("var",label= "Please Select Variable",
                  choices = c("Length of Stay"="V2", "Age"="V3", "Infection Risk"="V4","Routine Culturing Ratio"="V5",
                              "Routine Chest X ray Ratio"="V6"," Number of Beds"="V7",
                              "Average Daily Censu"="V10","Number of Nurses"="V11","Available Facilities Services"="V12")
    )),
    mainPanel(
      plotOutput("densPlot")
    )
  )
)

server <- function(input, output) {

  output$densPlot <- renderPlot({

      ggplot(dt, aes_string(x=input$var))+geom_density()+geom_histogram(aes(y = ..density..),bins=input$bins, alpha = 0.7,fill = "#6666FF",col="red")

  })


}

# Run the application
shinyApp(ui = ui, server = server)

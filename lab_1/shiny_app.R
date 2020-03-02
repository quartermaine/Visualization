library(shiny)
library(tidyverse)

df<-read.table("SENIC.txt")
dt<-df[c(-1,-8,-9)]
names(dt)


ui <- fluidPage(

  # Application title
  titlePanel("Density and Overlap Histogram Plot"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 100,
                  value = 30)
    ),selectInput("var",label= "Variable",
                  choices = c("Length of Stay", "Age", "Infection Risk","Routine Culturing Ratio","Routine Chest X-ray Ratio"," Number of Beds",
                              "Average Daily Censu","Number of Nurses","Available Facilities & Services"),

                  # Show a plot of the generated distribution
  mainPanel(plotOutput("distPlot")
                  )
    )
  )
)

server <- function(input, output) {

    output$distPlot <- reactivePlot({

      if (input$var=="Lenght if Stay"){
       plot<- ggplot(dt, aes_string(x=V2))+stat_density(alpha=0.8, position="identity")
      }
      if (input$var=="Age"){
        plot<-ggplot(dt, aes_string(x=V3))+stat_density(alpha=0.8, position="identity")
      }
      if (input$var=="Infection Risk"){
        plot<-ggplot(dt, aes_string(x=V4))+stat_density(alpha=0.8, position="identity")
      }
      if (input$var=="Routine Culturing Ratio"){
        plot<-ggplot(dt, aes_string(x=V5))+stat_density(alpha=0.8, position="identity")
      }
      if (input$var=="Routine Chest X-ray Ratio"){
        plot<-ggplot(dt, aes_string(x=V6))+stat_density(alpha=0.8, position="identity")
      }
      if (input$var=="Number of Beds"){
        plot<-ggplot(dt, aes_string(x=V7))+stat_density(alpha=0.8, position="identity")
      }
      if (input$var=="Average Daily Cens"){
        plot<-ggplot(dt, aes_string(x=V10))+stat_density(alpha=0.8, position="identity")
      }
      if (input$var=="Number of Nurses"){
        plot<-ggplot(dt, aes_string(x=V11))+stat_density(alpha=0.8, position="identity")
      }
      if (input$var=="Available Facilities & Services"){
        plot<-ggplot(dt, aes_string(x=V12))+stat_density(alpha=0.8, position="identity")
      }
      print(plot)


    })
  }

# Run the application
shinyApp(ui = ui, server = server)



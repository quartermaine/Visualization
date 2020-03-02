library(tidyverse)
library(plotly)
library(plyr)
library(crosstalk)
library(GGally)

olive<-read.csv("olive.csv")
olive$Region<-as.factor(olive$Region)
levels(olive$Region)<-c("North","South","Sardinia island")
o<-SharedData$new(olive)

###1
scatterolive <- plot_ly(o, x = ~eicosenoic, y = ~ linoleic) %>%
  add_markers(color = I("purple"))

scatterolive

###2

barolive <-plot_ly(o, x=~Region)%>%add_histogram()%>%layout(barmode="overlay")

#sub<-subplot(scatterolive,barolive)%>%
  #highlight(on="plotly_select", dynamic=T, persistent = T, opacityDim = I(1))%>%hide_legend()



bscols(widths=c(2, NA),filter_slider("Slider", "Stearic", o, ~stearic)
       ,subplot(scatterolive,barolive)%>%
         highlight(on="plotly_select", dynamic=T, persistent = T, opacityDim = I(1))%>%hide_legend())

###3
#scatter plots eicosenoic against linoleic and arachidic against linolenic
scatter1 <- plot_ly(o, x = ~eicosenoic, y = ~linolenic) %>%
  add_markers(color = I("black"))
scatter2 <- plot_ly(o, x = ~arachidic, y = ~linolenic) %>%
  add_markers(color = I("black"))


subplot(scatter1,scatter2)%>%
  highlight(on="plotly_select", dynamic=T, persistent=T, opacityDim = I(1))%>%hide_legend()

###4

#Create a parallel coordinate plot for the available eight acids, a linked 3d-scatter plot in
#which variables are selected by three additional drop boxes and a linked bar chart
#showing Region



p<-ggparcoord(olive,columns = c(4:11))

d<-plotly_data(ggplotly(p))%>%group_by(.ID)
d1<-SharedData$new(d, ~.ID, group="olive")
p1<-plot_ly(d1, x=~variable, y=~value)%>%
  add_lines(line=list(width=0.3))%>%
  add_markers(marker=list(size=0.3),
              text=~.ID, hoverinfo="text")


#olive2=olive[, c(4:11)]
#olive2$.ID=1:nrow(olive)
#d2<-SharedData$new(olive2, ~.ID, group="olive")


ButtonsX=list()
for (i in 4:11){
  ButtonsX[[i-3]]= list(method = "restyle",
                        args = list( "x", list(olive[[i]])),
                        label = colnames(olive)[i])
}

ButtonsY=list()
for (i in 4:11){
  ButtonsY[[i-3]]= list(method = "restyle",
                        args = list( "y", list(olive[[i]])),
                        label = colnames(olive)[i])
}

ButtonsZ=list()
for (i in 4:11){
  ButtonsZ[[i-3]]= list(method = "restyle",
                        args = list( "y", list(olive[[i]])),
                        label = colnames(olive)[i])
}

p3<-plot_ly(o,x=~palmitic,y=~stearic,z=~oleic)%>%add_markers() %>%
  add_markers() %>%
  layout(xaxis=list(title=""), yaxis=list(title=""),
         title = "Select variable:",
         updatemenus = list(
           list(y=0.9, buttons = ButtonsX),
           list(y=0.8, buttons = ButtonsY),
           list(y=0.7, buttons = ButtonsZ)
         )  )

p3


bscols(p1%>%highlight(on="plotly_select", dynamic=T, persistent = T, opacityDim = I(1))%>%
         hide_legend(),
       p3%>%highlight(on="plotly_click", dynamic=T, persistent = T)%>%hide_legend(),
       barolive)




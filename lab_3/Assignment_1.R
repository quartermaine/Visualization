library(tidyverse)
library(plotly)
library(RCurl)
library(gridExtra)
library(RColorBrewer)

mosquitos<-read.csv('aegypti_albopictus.csv',header=TRUE)

#####MAP PLOT FOR YEAR 2004

p_2004 <-mosquitos %>%filter(YEAR=='2004')%>%plot_mapbox(lat = ~Y, lon = ~X,
                                                         split=~VECTOR,mode = 'scattermapbox', hoverinfo='name')%>%
  layout(title = 'MOSQUITOS PLOS FOR YEAR 2004',
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))
p_2004
#####MAP PLOT FOR YEAR 2013

p_2013<-mosquitos %>%filter(YEAR=='2013')%>%plot_mapbox(lat = ~Y, lon = ~X,
                                                        split=~VECTOR,mode = "scattermapbox", hoverinfo='name')%>%
  layout(title = 'MOSQUITOS PLOT FOR YEAR 2013',
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))

p_2013



#####CHOROPLETH MAP Equirectangular Projection
d<-mosquitos%>%select(c("VECTOR","COUNTRY_ID"))
x<-group_by(d,COUNTRY_ID)%>%count()
s<-as.data.frame(x)

g1 <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'equirectangular')
)


p1 <- s%>% plot_geo()  %>%
  add_trace(
    z = ~n, color=~n,colors = 'Purples',
    text = ~COUNTRY_ID,locations=~COUNTRY_ID
  ) %>% colorbar(title = 'Total number of mosquitos')%>%
  layout(title = 'Choropleth Plot of Mosquitos per Country',
    geo = g1
  )

p1

#####CHOROPLETH MAP log(Z) Equirectangular Projection

s$log_n<-log(s$n)

g2 <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = "equirectangular")
)


p2 <- s%>% plot_geo()  %>%
  add_trace(
    z = ~log_n, color=~log_n,colors = 'Purples',
    text = ~COUNTRY_ID,locations=~COUNTRY_ID
  ) %>% colorbar(title = 'Logarithmic Total number of mosquitos')%>%
  layout(title = 'Choropleth Plot of Mosquitos per Country \n with Log Transformation',
         geo = g2
  )

p2

#####CHOROPLETH MAP log(Z) Conic Equal Projection

s$log_n<-log(s$n)

g3 <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = "conic equal area")
)


p3 <- s%>% plot_geo()  %>%
  add_trace(
    z = ~log_n, color=~log_n,colors = 'Purples',
    text = ~COUNTRY_ID,locations=~COUNTRY_ID
  ) %>% colorbar(title = 'Logarithmic Total number of mosquitos')%>%
  layout(title = 'Choropleth Plot of Mosquitos per Country \n with Log Transformation',
         geo = g3
  )

p3


#####
mosquitos_brazil<-mosquitos[(mosquitos$COUNTRY == "Brazil" & mosquitos$YEAR =="2013" ), ]

mosquitos_brazil$X1<-cut_interval(mosquitos_brazil$X,100)
mosquitos_brazil$Y1<-cut_interval(mosquitos_brazil$Y,100)


#mos_group_X1<-as.data.frame(mosquitos_brazil%>%group_by(X1)%>%summarise(mean_group_X=mean(X),n_group_X=n()))
#mos_group_Y1<-as.data.frame(mosquitos_brazil%>%group_by(Y1)%>%summarise(mean_group_Y=mean(Y),n_group_Y=n()))


mos<-as.data.frame(mosquitos_brazil)%>%group_by(X1,Y1)%>%summarise(m1=mean(X),m2=mean(Y),N=count(VECTOR))
mos<-as.data.frame(mos)

br <-mos %>%plot_mapbox(lat = ~m2, lon = ~m1,color = ~N ,mode = 'scattermapbox', hoverinfo='name')%>%
  layout(title = 'Mean values of X and Y per group (X1,Y1) and amount of obs per group (X1,Y1) ',
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))
br


mosquitos$X_cut = cut_interval(mosquitos$X, n = 100, labels=FALSE)
mosquitos$Y_cut = cut_interval(mosquitos$Y, n = 100, labels=FALSE)

mosquitos %>% filter(YEAR == "2013" & COUNTRY == "Brazil") %>% group_by(COUNTRY, X_cut, Y_cut) %>% 
  summarise(mean_group_X = mean(X), mean_group_Y = mean(Y), count=n()) %>% 
  plot_mapbox(lon  = ~mean_group_X, lat = ~mean_group_Y, mode = 'scattermapbox',
               hoverinfo='count', width = 800, 
              height = 800, color = ~count) %>% layout(title = 'Brazil mosquito Population') 




#####Violin plots

###method to write csv
#url <- paste("http://www.statistikdatabasen.scb.se/sq/56793")
#download.file(url, destfile = "swedish_data.csv" )
#####

#custom <-function(x){
#if (swedish_wide$Region[x]=="01 Stockholm county"){
#swedish_wide$Region[x]<-"Stockholm'"
#}
#else if(swedish_wide$Region[x]=="01 Stockholm county"){
#return(" ")
#}
#}

#lapply(swedish_wide$Region,custom)

#options(encoding = "UTF-8")
#myfile <- getURL("http://www.statistikdatabasen.scb.se/sq/56793", ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
#swedish<-read.csv2(textConnection(myfile), header=TRUE,fileEncoding="UTF-8",stringsAsFactors=FALSE,sep=",")

swedish<-read.csv("swedish_data.csv",sep=",",encoding="UTF-8",quote = "", header=TRUE)


swedish_wide <- spread(swedish,X.age.,X.2016.)

names(swedish_wide)<-c("Region","Young","Adult","Senior")





v1 <- swedish_wide %>%plot_ly(y = ~Young,type = 'violin',box = list(visible = TRUE),meanline = list(visible = TRUE),x0 = 'Mean Income for Youngs') %>% 
  layout(yaxis = list(title = "Mean Income for the Young Age Group",zeroline = FALSE))

#v1


v2 <- swedish_wide %>%plot_ly(y = ~Adult,type = 'violin',box = list(visible = TRUE),meanline = list(visible = TRUE),x0 = 'Mean Income for Adult') %>% 
  layout(yaxis = list(title = "Mean Income for the Adult Age Group",zeroline = FALSE))

#v2


v3 <- swedish_wide %>%plot_ly(y = ~Adult,type = 'violin',box = list(visible = TRUE),meanline = list(visible = TRUE),x0 = 'Mean Income for Senior') %>% 
  layout(title = list(title = "Mean Income for the Senior Age Group",zeroline = FALSE))

#v3
v4 <- swedish_wide %>%plot_ly(y = ~Adult,type = 'violin',box = list(visible = TRUE),meanline = list(visible = TRUE),x0 = 'Mean Income for Senior') %>% 
  layout(title = list(title = "Mean Income for the Senior Age Group",zeroline = FALSE))%>%

v4

subplot(v1,v2,v3)


ee <- swedish_wide %>%
  plot_ly(type = 'violin') %>%
  add_trace(y = ~Young,name = 'Young',box = list(visible = T),
    meanline = list(visible = T),line = list(color = 'pink')
  )%>%
  add_trace(y = ~Adult,name = 'Adults',box = list(visible = T),
            meanline = list(visible = T),line = list(color = 'blue')) %>% 
  add_trace(
    y = ~Senior,name = 'Senior',box = list(visible = T),
    meanline = list(visible = T),line = list(color = 'green')
  )%>%
  layout(yaxis = list(title = "income ", 
                      zeroline = F), xaxis = list(title = "Age Group"), title = "Income vs  Age Group"
  )


ee
#####Surface Plot



library(akima)

surface_plot=interp(swedish_wide$Young, swedish_wide$Adult, swedish_wide$Senior, duplicate = "mean")

plot_ly(x=~surface_plot$x, y=~surface_plot$y, z=~surface_plot$z, type="surface") %>%  layout(
  title = "Senior incomes on Adult and Young incomes",
  scene = list(
    xaxis = list(title = "Young Income"),
    yaxis = list(title = "Adult Income"),
    zaxis = list(title = "Senior Income")
  ))


#####Choropleth maps
map<-readRDS("gadm36_SWE_1_sf.rds")



swedish_wide<- swedish_wide %>% separate(Region, c("region_no", "region", "type"), " ")
swedish_wide$region <- as.character(swedish_wide$region)

map <- inner_join(x = map[,c("NAME_1", "geometry")], y = swedish_wide, by=c("NAME_1" = "region"))

plot_ly() %>% add_sf(data=map, split=~NAME_1, color=~Young, showlegend=F, alpha=1, type = "scatter") %>% layout(title = "Choropleth of youth income")


rownames(swedish_wide)=swedish_wide$Region
map$Young=swedish_wide$Young
#Data for some regions absent, setting to 0
map$Young[is.na(map$Young)]=0

#plotly
j1<-plot_ly()%>%add_sf(data=map, split=~NAME_1, color=~Young, showlegend=F, alpha=1,mode="markers",type="scatter")%>%layout(
  title = "Choropleth Map of Youngs Income in Sweden")

j1


map$Adult=swedish_wide$Adult
#Data for some regions absent, setting to 0
map$Adult[is.na(map$Adult)]=0

#plotly
j2<-plot_ly()%>%add_sf(data=map, split=~NAME_1, color=~Adult, showlegend=F, alpha=1,mode="markers",type="scatter")%>%layout(
  title = "Choropleth Map of Adults Income in Sweden")

j2

library(sf)
lat= 58.409814
longitude = 15.624525
name = "Linkoping"
desc = "Linköping, Östergötlands län, SE"

data_point <- data.frame(lat, longitude, name, desc)
plot_ly() %>% add_sf(data=map, split=~NAME_1, color=~Young, showlegend=F, alpha=1, 
                     type = "scatter") %>% layout(title = "Choropleth of Income of Young") %>% add_markers(data = data_point,
                                                                                                           y = ~lat, x = ~longitude, color = I("red"), text='Linkoping')







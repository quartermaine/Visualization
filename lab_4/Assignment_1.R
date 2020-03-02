library(tidyverse)
library(RColorBrewer)
library(plotly)
library(seriation)

df<-read.csv("prices-and-earnings.csv")

#names(df)<-c("City","Food.Costs","Womens.Clothing","Mens.Clothing","iPhone.4S(hr)","Clothing.Index","Hours.Worked",	
               #"Wage.Gross","Wage.Net","Vacation.Days",	"COL(Excl.rent)",	"COL.(incl.rent)"	,"Pur.Power.Gross",	
               #"Pur.Power.Net",	"Pur.Power.Annual",	"Big.Mac(min)",	"Bread(kg.in.min)",	"Rice(kg.in.min)","Goods.and.Services",	
                #"Good.and.Services.Index","Food.Index")

prices<-df[c(1,2,5,6,7,9,10,16,17,18,19)+1]
rownames(prices)<-df[,1]
#prices$`Food Costs($)`<-as.numeric(prices$`Food Costs($)`)
###HEATMAP 
prices_m<-as.matrix(prices)

#p1<-plot_ly(x=colnames(prices_m), y=rownames(prices_m), z=prices_m, type="heatmap")
#p1

coul = colorRampPalette(brewer.pal(8, "PiYG"))(25)

#heatmap(as.matrix(prices), scale="column", col = coul)
adscaled=scale(prices_m) 
p2<-plot_ly(x=colnames(adscaled), y=rownames(adscaled), z=adscaled, type="heatmap", colors = coul)
p2

###Heat Map with Euclidean Distance HC

rowdist1<-dist(adscaled,method = "minkowski", p=2)
coldist1<-dist(t(adscaled),method = "minkowski", p=2)
order1<-seriate(rowdist1, "HC")
order2<-seriate(coldist1, "HC")
ord1<-get_order(order1)
ord2<-get_order(order2)
reord_e<-prices_m[rev(ord1),ord2]
plot_ly(x=colnames(reord_e), y=rownames(reord_e), 
        z=reord_e, type="heatmap", colors =colorRamp(c("yellow", "red")))

res_e<-criterion(rowdist1, order1)

###Heat Map with 1-Correlation HC
coldist2<-as.dist(1-cor(adscaled))
rowdist2<-as.dist(1-cor(t(adscaled)))
order1_c<-seriate(rowdist2, "HC")
order2_c<-seriate(coldist2, "HC")
ord1_c<-get_order(order1_c)
ord2_c<-get_order(order2_c)
reord_c<-prices_m[rev(ord1_c),ord2_c]
plot_ly(x=colnames(reord_c), y=rownames(reord_c), 
        z=reord_c, type="heatmap", colors =colorRamp(c("lightskyblue", "limegreen")))

###Heat Map with Euclidean Distance TSP
rowdist3<-dist(adscaled,method = "minkowski", p=2)
coldist3<-dist(t(adscaled),method = "minkowski", p=2)
order1_tsp<-seriate(rowdist3, "TSP")
order2_tsp<-seriate(coldist3, "TSP")
ord1_tsp<-get_order(order1_tsp)
ord2_tsp<-get_order(order2_tsp)
reord_tsp<-prices_m[ord1_tsp,ord2_tsp]
plot_ly(x=colnames(reord_tsp), y=rownames(reord_tsp), 
        z=reord_tsp, type="heatmap", colors =colorRamp(c("yellow", "darkblue")))

res_tsp<-criterion(rowdist3, order1_tsp)
###Parallel Coordinate Plot for Unsorted data
v<-1:11
ord=sample(v)
#ord=ord2
dims0=list()
for( i in 1:ncol(prices)){
  dims0[[i]]=list( label=colnames(prices)[ord[i]],
                   values=as.formula(paste("~",colnames(prices)[ord[i]])))
}
p <-prices%>%
  plot_ly(type = 'parcoords',
          line = list(color = ~as.numeric(Hours.Worked)),dimensions = dims0)
p


###Radar Plots

Ps=list()
nPlot=72

prices[ord1,] %>%
  add_rownames( var = "group" ) %>%
  mutate_each(funs(rescale), -group) -> prices_radar

for (i in 1:nPlot){
  Ps[[i]] <- htmltools::tags$div(
    plot_ly(type = 'scatterpolar',  
            r=as.numeric(prices_radar[i,-1]),
            theta= colnames(prices_radar)[-1], 
            fill="toself")%>%
      layout(title=prices_radar$group[i]), style="width: 20%;")  # 4 plots per row
}
h <-htmltools::tags$div(style = "display: flex; flex-wrap: wrap", Ps)
htmltools::browsable(h)





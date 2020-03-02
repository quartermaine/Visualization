##########################################################################################
#############################Assignment 2-2###############################################
library(readxl)
library(tidyverse)
library(plotly)
library(MASS)


df<-read_xlsx("baseball-2016.xlsx",col_names=TRUE)
scaled_df<-scale(df[,-c(1,2)])
d<-dist(scaled_df)
mds<-isoMDS(d,k=2)

dt<-as.data.frame(mds$points)
dt$League<-df$League
rownames(dt)<-df$Team



#plot_ly(dt, x = ~V1, y= ~V2,type ="scatter", color = ~League,labels=rownames(dt))

p <- ggplot(data=dt,aes(x=V1,y=V2,col=League,size=5))+geom_point()+theme_minimal()+geom_text(aes(label=rownames(dt),size=5))
ggplotly(p)


############################################################################################
#############################Assignment 2-3#################################################
sh <- Shepard(d,dt)
delta <-as.numeric(d)
D<- as.numeric(dist(dt))

n=nrow(dt)
index=matrix(1:n, nrow=n, ncol=n)
index1=as.numeric(index[lower.tri(index)])

n=nrow(dt)
index=matrix(1:n, nrow=n, ncol=n, byrow = T)
index2=as.numeric(index[lower.tri(index)])



plot_ly()%>%
  add_markers(x=~delta, y=~D, hoverinfo = 'text')%>%add_lines(x=~sh$x, y=~sh$yf)



############################################################################################
#############################Assignment 2-4#################################################






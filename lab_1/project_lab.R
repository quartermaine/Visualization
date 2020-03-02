library(tidyverse)
library(ggplot2)
library(gridExtra)
df<-read.table("SENIC.txt")

my_func<-function(x,name){
  col=x[[name]]
  quantile_1=quantile(col,0.25)
  quantile_3=quantile(col,0.75)
  l1=quantile_3+1.5*(quantile_3-quantile_1)
  l2=quantile_1-1.5*(quantile_3-quantile_1)
  indices=which(col>=l1 | col<=l2)
  #stack=c(col,indices)
  return(indices)
}

indices_infection<-my_func(df,"V3")
outliers_infection<-df$V3[indices_infection]
outliers <- tibble(x = outliers_infection, y = 0)

g<-ggplot(df,aes(x=V3))+geom_histogram(aes(y = ..density..),bins=50, alpha = 0.7,fill = "#6666FF")+geom_density(col="#330066")
g<-g+geom_point(data = outliers, aes(x,y),size=2,colour="#00CCFF",pch=23,fill="#00CCFF")+ggtitle("Density & Histogram Plot of Infection Risk")+labs(x="Infection Risk",y="Density")
g<-g+theme(plot.title = element_text(color="#666666",size=21, hjust=0))
g+annotate(geom="text",x=64,y=0.07,label="Diamond points \n represent outliers")


dt<-df[c(-1,-8,-9)]
names(dt)<-c('LengthofStay','Age','InfectionRisk','RoutineCulturingRatio',
                    'RoutineChestXrayRatio','NumberofBeds','AverageDailyCensus',
                    'NumberofNurses','AvailableFacilitiesServices')

myplots<-list()

for (name in names(dt))
{
  
  indices<-my_func(dt,name)
  outliers<-dt[[name]][indices]
  outliers_names<-tibble(x=outliers,y=0)
  myplots[[name]]<-ggplot(dt, aes_string(x =name)) + geom_density(col="#330066")+geom_histogram(aes(y=..density..),bins=50, alpha = 0.7,fill = "#6666FF")+geom_point(data=outliers_names,aes(x,y),size=2,color="#00CCFF",pch=23,fill="#00CCFF")
}
grid.arrange(grobs=myplots)

#Scatterplot
ggplot(df,aes(y=V10,x=V3,col=V6))+geom_point(size=5)+ggtitle("Scatter Plot of Infection Rsk & Number of Nurses colored by Number of Beds")+labs(x="Infection Risk",y="Number of Nurses")


#Density Plot of Infection Risk with Plotly
library(plotly)

plot<-ggplot(df,aes(V3))+geom_histogram(aes(y=..density..,alpha=0.7),col="coral",fill="green")+geom_density(col="blue")+ggtitle("Density with Histogram overlay")+geom_point(data = outliers, aes(x,y),sizze=3)
p<-ggplotly(plot)

p
 
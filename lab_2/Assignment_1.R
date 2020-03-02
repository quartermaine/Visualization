library(tidyverse)
library(MASS)

olive<-read.csv("olive.csv",header=TRUE, sep=",")

###############Assignment-1_1
#Create a scatterplot in Ggplot2 that shows dependence of Palmitic on Oleic in which observations are colored by Linolenic

p1<-ggplot(olive,aes(palmitic,oleic,col=linolenic))+geom_point()+labs(x="palmitic",y="oleic")+geom_point(size=2)+ggtitle("Plot of Palmitic on Oleic coloured by Linolenic")
p1

interval_cut <- cut_interval(olive$linolenic, 4)
p2<-ggplot(olive,aes(palmitic,oleic,col=interval_cut,shape=interval_cut))+geom_point()+theme_light()+labs(x="palmitic",y="oleic")+geom_point(size=3)+ggtitle("Plot of Palmitic on Oleic coloured by Linolenic")+theme(axis.text = element_text(colour = "blue"))
p2

###############Assignment-1_2
#Create scatterplots of Palmitic vs Oleic in which you map the discretized Linolenic with four classes to: 
#a. Color b. Size c. Orientation angle (use geom_spoke()) 

p3<-s2<-ggplot(olive,aes(palmitic,oleic,col=interval_cut))+geom_point()+theme_light()+labs(x="palmitic",y="oleic")+geom_point(size=3)+ggtitle("Plot of Palmitic on Oleic coloured by Linolenic")+theme(axis.text = element_text(colour = "blue"))
p3

p4<-s2<-ggplot(olive,aes(palmitic,oleic))+geom_point(aes(size=interval_cut))+theme_light()+labs(x="palmitic",y="oleic")+geom_point(size=3)+ggtitle("Plot of Palmitic on Oleic coloured by Linolenic")+theme(axis.text = element_text(colour = "blue"))
p4

p5<-ggplot(olive,aes(x=palmitic,y=oleic)) +geom_point() +geom_spoke(aes(angle = 90, radius = 45))
p5

###############Assignment-1_3
#Create a scatterplot of Oleic vs Eicosenoic in which color is defined by numeric values of Region.

p6<-ggplot(olive,aes(x=oleic,y=eicosenoic,color=as.numeric(Region)))+labs(x="oleic",y="eicosenoic")+geom_point(size=2)+ggtitle("oleic on eicosenoic coloured by Region")
p6

p7<-ggplot(olive,aes(x=oleic,y=eicosenoic,color=as.factor(Region)))+labs(x="oleic",y="eicosenoic")+geom_point(size=2)+ggtitle("oleic on eicosenoic coloured by Region")
p7

###############Assignment-1_4

#Create a scatterplot of Oleic vs Eicosenoic in which color is defined by a discretized Linoleic (3 classes), 
#shape is defined by a discretized Palmitic (3 classes) and size is defined by a discretized Palmitoleic (3 classes)

discretelionelic<-cut_interval(olive$linolenic, 3)
discretepalmitic<-cut_interval(olive$palmitic, 3)
discretepalmitoleic<-cut_interval(olive$palmitoleic, 3)

p8<-ggplot(olive,aes(x=oleic,y=eicosenoic,color=discretelionelic,shape=discretepalmitic,size=discretepalmitoleic))+geom_point()+ggtitle("oleic vs eicosenoic with linolenic, palmitic,palmitoleic")
p8
###############Assignment-1_5
##Create a scatterplot of Oleic vs Eicosenoic in which color is defined by Region, 
#shape is defined by a discretized Palmitic (3 classes) and size is defined by a discretized Palmitoleic (3 classes)

p9<-ggplot(olive,aes(x=oleic,y=eicosenoic,color=Region,shape=discretepalmitic,size=discretepalmitoleic))+geom_point()+ggtitle("oleic vs eicosenoic with linolenic, palmitic,palmitoleic")
p9

###############Assignment-1_6
#Use Plotly to create a pie chart that shows the proportions of oils coming from different Areas
library(plotrix)

mytable <- table(olive$Area)
dframe<-as.data.frame(mytable)

#pie3D(dframe$Freq,labels=dframe$Var1,explode=0.1,main="Pie Chart of Area Proportion ")

p10 <- plot_ly(dframe, labels = ~Var1, values = ~Freq, type = 'pie') %>%
  layout(title = 'Pie chart of Area Proportion',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p10

###############Assignment-1_7
#Create a 2d-density contour plot with Ggplot2 in which you show dependence of Linoleic vs Eicosenoic


p11<-ggplot(olive, aes(x=linoleic, y=eicosenoic,col="red") ) +geom_density_2d(position="identity")+theme_dark()
p11


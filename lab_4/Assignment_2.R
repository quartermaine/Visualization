library(tidyverse)
library(RColorBrewer)
library(plotly)


adults<-read.csv("adult.csv",sep=",")
names(adults)<-c("Age","Workclass","PolpulationIndex","Education","EducationNum","MaritalStatus","Occupation","Relationship",
                 "Race","Sex","CapitalGain","CapitalLoss","HoursPerWeek","NativeCountry","IncomeLevel")

### scatter plot of Hours per Week versus age where observations are colored by Income level. 
# Make a trellis plot of the same kind where you condition on Income Level

sc1<-ggplot(adults,aes(x=HoursPerWeek,y=Age,col=IncomeLevel))+geom_point()
sc1

tr1<-ggplot(adults,aes(x=HoursPerWeek,y=Age,color=IncomeLevel))+geom_point()+facet_grid(IncomeLevel~.)+theme_bw()+theme(strip.background = element_rect(colour="red", fill="#CCCCFF"))+geom_smooth()
tr1

#The problem with the scaterplot is that we cannot distinguist the two income Levels because there are lot of points overlaping.
#In contrast with the trellis plot we can easily see the trend for the 2 Income categories. 

##Use ggplot2 to create a density plot of age grouped by the Income level. 
##Create a trellis plot of the same kind where you condition on Marital Status


d1<-ggplot(adults,aes(x=Age))+geom_density(aes(fill=IncomeLevel,color=IncomeLevel),alpha=0.4)+ggtitle("Density Plot")
d1


d2<-ggplot(adults,aes(x=Age))+
  geom_density(aes(col=IncomeLevel,fill=IncomeLevel),alpha=0.4)+facet_wrap(~MaritalStatus, labeller = "label_both")+ggtitle("Trellis Plot ")
d2

#From the first plot we can see that people that earn less that 50k is mostly young people and people earn more than 50k is more likely 
#to be seniors.
#From the second plot we can see that people are widowed 


##Filter out all observations having Capital loss equal to zero. For the remaining data, 
##use Plotly to create a 3D-scatter plot of Education-num vs Age vs Captial Loss

pp <- adults%>%filter(CapitalLoss!=0)%>%plot_ly( x = ~EducationNum, y = ~Age, z = ~CapitalLoss,color=~EducationNum) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'EducationNum'),
                      yaxis = list(title = 'Age'),
                      zaxis = list(title = 'CapitalLoss')))
pp

###Create a trellis plot with 6 panels in ggplot2 in which each panel shows a raster-type 2d-density plot of Capital Loss 
####versus Education-num conditioned on values of Age (use cut_number())


tr2<-adults[which(adults$CapitalLoss!=0),]%>%ggplot(aes(x=CapitalLoss,y=EducationNum))+geom_density_2d()+facet_wrap(~cut_number(Age,n=6))
tr2+ggtitle("Trellis 2d-Density Plots")+theme_gray()%+replace%theme(plot.background = element_rect(fill = "aliceblue"))





###Make a trellis plot containing 4 panels where each panel should show a scatter plot of Capital Loss versus Education-num 
###conditioned on the values of Age by a) using cut_number() b) using Shingles with 10% overlap. 


#a)
adults%>%filter(CapitalLoss!=0)%>%ggplot(aes(x=CapitalLoss,y=EducationNum))+geom_point()+facet_wrap(~cut_number(Age,n=4))


#b)

#cap_adults<-adults[which(adults$CapitalLoss!=0),]
Agerange<-lattice::equal.count(adults$Age, number=4, overlap=0.10) #overlap is 10% 
L<-matrix(unlist(levels(Agerange)), ncol=2, byrow = T)
L1<-data.frame(Lower=L[,1],Upper=L[,2], Interval=factor(1:nrow(L)))

index=c() 
Class=c() 
for(i in 1:nrow(L)){ 
  Cl=paste("[", L1$Lower[i], ",", L1$Upper[i], "]", sep="") 
  ind=which(adults$Age>=L1$Lower[i] & adults$Age<=L1$Upper[i]) 
  index=c(index,ind) 
  Class=c(Class, rep(Cl, length(ind))) } 


df4<-adults[index,]
df4$Class<-as.factor(Class) 
h1<-ggplot(df4, aes(x=CapitalLoss,y=EducationNum))+ geom_point()+ facet_wrap(~Class, labeller = "label_both") 
h1







